{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoFieldSelectors           #-}
{-# LANGUAGE OverloadedRecordDot        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Pure unit tests for TaskMap logic: validateTaskMap, replaceTasks and
-- placeholder tasks. Runs locally, no cluster or filesystem access needed.
module Hyperion.Scheduler.Test.TaskMapTest where

import Control.Exception                        (AssertionFailed (..), throwIO)
import Control.Monad                            (unless)
import Data.Aeson                               (ToJSON)
import Data.Binary                              (Binary)
import Data.List.NonEmpty                       qualified as NonEmpty
import Data.Map.Strict                          (Map)
import Data.Map.Strict                          qualified as Map
import Data.Maybe                               (isNothing)
import Data.Set                                 (Set)
import Data.Set                                 qualified as Set
import Data.Text                                qualified as Text
import GHC.Generics                             (Generic)
import Hyperion.OsPath                          (OsPath)
import Hyperion.OsString                        (fromString)
import Hyperion.Scheduler.FilePath              (VirtualFilePath (..))
import Hyperion.Scheduler.StatKey               (FileStatKey,
                                                 IsFileStatKey (..),
                                                 TaskKeyFileInfo (..),
                                                 ToFileStatKey (..),
                                                 encodeFileStatKey, unitSummary)
import Hyperion.Scheduler.Stats                 (FileStats (..),
                                                 TaskAndFileStats (..),
                                                 TaskStats (..), toTrials)
import Hyperion.Scheduler.Task.EstimatedTaskMap (estimatedTasks,
                                                 mkEstimatedTaskMap)
import Hyperion.Scheduler.Task.IsTask           (IsTask (..), TaskSummary (..),
                                                 filesOnlySummary,
                                                 taskInputPaths, taskInputs)
import Hyperion.Scheduler.Task.Task             (DepKeys, TaskKey (..),
                                                 TaskKind (..), dependencies,
                                                 listTaskKey, outKeys)
import Hyperion.Scheduler.Task.TaskMap          (InstrumentationGap (..),
                                                 TaskMap, placeholdersOfType,
                                                 replaceTasks,
                                                 taskInstrumentationGaps,
                                                 validateTaskMap)
import Hyperion.Scheduler.Task.WrappedTask      (wrapTask)
import Hyperion.Scheduler.TaskFiles             (MonadTaskFiles (..))
import Hyperion.Scheduler.Types                 (Estimate (..), FileSize,
                                                 NumCPUs, schedulingEstimate)

-- * A minimal IsTask for building TaskMaps by hand

data TestTask = MkTestTask
  { name          :: String
  , inputs        :: Set OsPath
  , outputs       :: Set OsPath
  , isPlaceholder :: Bool
  , computes      :: Bool
    -- ^ Whether the task has something to run remotely. The closure itself is
    -- never forced: these tests only ask whether one exists.
  , minThreads    :: NumCPUs
  , maxThreads    :: NumCPUs
  } deriving (Eq, Ord, Show, Generic, ToJSON)

-- | A file's identity in these fixtures is just its path.
newtype PathFileStatKey = MkPathFileStatKey String
  deriving newtype (ToJSON)

instance IsFileStatKey PathFileStatKey

fileStatKeyOfPath :: OsPath -> FileStatKey
fileStatKeyOfPath path = encodeFileStatKey (MkPathFileStatKey (show path))

mkFileInfo :: OsPath -> TaskKeyFileInfo
mkFileInfo path = MkTaskKeyFileInfo
  { fileStatKey = Just $ fileStatKeyOfPath path
  , path        = VirtualFilePath path
  , fileSize    = EstimatedByTask 0
  }

instance IsTask TestTask where
  taskSummary knownInputs t =
    filesOnlySummary (Set.map mkFileInfo t.inputs) (Set.map mkFileInfo t.outputs) knownInputs
  taskTag t           = Just (Text.pack t.name)
  taskClosure t
    | t.computes = Just $ error "TestTask closure is never run"
    | otherwise  = Nothing
  taskMinThreads _ t  = t.minThreads
  taskMaxThreads _ t  = t.maxThreads
  taskIsPlaceholder t = t.isPlaceholder

testTask :: String -> [OsPath] -> [OsPath] -> TestTask
testTask name ins outs = MkTestTask
  { name          = name
  , inputs        = Set.fromList ins
  , outputs       = Set.fromList outs
  , isPlaceholder = False
  , computes      = False
  , minThreads    = 1
  , maxThreads    = 1
  }

-- * A placeholder TaskKey, exercising the real 'TaskKind' machinery

newtype PKey = MkPKey String
  deriving newtype (Eq, Ord, Show, Binary, ToJSON)

type instance DepKeys PKey = '[]

-- A placeholder has no outputs, so it declares no file stat key.
instance ToFileStatKey PKey

instance TaskKey PKey where
  taskKind = PlaceholderTask

-- * Assertions

expect :: String -> Bool -> IO ()
expect label cond = do
  unless cond $ throwIO $ AssertionFailed $ "FAILED: " <> label
  putStrLn $ "ok: " <> label

taskMapOf :: TestTask -> TaskMap TestTask
taskMapOf t = Map.fromList [(t, Set.empty)]

expectValid :: String -> TaskMap TestTask -> IO ()
expectValid label taskMap = case validateTaskMap taskMap of
  Right () -> putStrLn $ "ok: " <> label
  Left err -> throwIO $ AssertionFailed $ "FAILED: " <> label <> ": unexpectedly invalid: " <> show err

expectInvalid :: String -> TaskMap TestTask -> IO ()
expectInvalid label taskMap = case validateTaskMap taskMap of
  Left _   -> putStrLn $ "ok: " <> label
  Right () -> throwIO $ AssertionFailed $ "FAILED: " <> label <> ": unexpectedly valid"

-- * Tests

p1, p2, q, r, s :: OsPath
p1 = fromString "/data/p1"
p2 = fromString "/data/p2"
q  = fromString "/data/q"
r  = fromString "/data/r"
s  = fromString "/data/s"

-- | An input path produced by NO task in the map is assumed to already exist
-- on disk (its producer was pruned by checkCreated): valid.
testPrunedInputIsValid :: IO ()
testPrunedInputIsValid = do
  let
    a = testTask "A" [p1] [q]
    taskMap = Map.fromList [(a, Set.empty)]
  expectValid "input with no producer in map (pruned dependency) is valid" taskMap

-- | An input path produced by a task IN the map, with no dependency edge to
-- it: invalid (this is the broken-graph case).
testMissingEdgeIsInvalid :: IO ()
testMissingEdgeIsInvalid = do
  let
    a = testTask "A" [p1] [q]
    b = testTask "B" [] [p1]
    taskMap = Map.fromList [(a, Set.empty), (b, Set.empty)]
  expectInvalid "input produced in map but not by dependencies is invalid" taskMap

-- | The same graph with the edge present: valid.
testCorrectEdgeIsValid :: IO ()
testCorrectEdgeIsValid = do
  let
    a = testTask "A" [p1] [q]
    b = testTask "B" [] [p1]
    taskMap = Map.fromList [(a, Set.singleton b), (b, Set.empty)]
  expectValid "input produced by a dependency is valid" taskMap

-- | Unreplaced placeholder tasks are rejected.
testUnreplacedPlaceholderIsInvalid :: IO ()
testUnreplacedPlaceholderIsInvalid = do
  let
    d = (testTask "D" [] [p1]) { isPlaceholder = True }
    a = testTask "A" [p1] [q]
    taskMap = Map.fromList [(a, Set.singleton d), (d, Set.empty)]
  expectInvalid "unreplaced placeholder is invalid" taskMap

-- | replaceTasks connects dependents only to the roots of the replacement
-- fragment, not to every task in it.
testReplaceTasksConnectsToRootsOnly :: IO ()
testReplaceTasksConnectsToRootsOnly = do
  let
    d = (testTask "D" [] [p1]) { isPlaceholder = True }   -- placeholder for W
    a = testTask "A" [p1] [q]                             -- consumer
    w = testTask "W" [r] [p1]                             -- fragment root
    x = testTask "X" [] [r]                               -- fragment dependency
    taskMap  = Map.fromList [(a, Set.singleton d), (d, Set.empty)]
    fragment = Map.fromList [(w, Set.singleton x), (x, Set.empty)]
    replaced = replaceTasks (Map.singleton d fragment) taskMap
  expect "placeholder removed from keys" $ not (Map.member d replaced)
  expect "consumer depends exactly on the fragment root" $
    Map.lookup a replaced == Just (Set.singleton w)
  expect "fragment edges preserved" $
    Map.lookup w replaced == Just (Set.singleton x) && Map.lookup x replaced == Just Set.empty
  expectValid "replaced map is valid" replaced

-- | PlaceholderTask semantics at the TaskKey level: output is definitionally
-- the key itself, no dependencies, nothing to forget.
testPlaceholderTaskKeySemantics :: IO ()
testPlaceholderTaskKeySemantics = do
  let key = MkPKey "some-block"
  expect "placeholder outKeys is the key itself" $
    outKeys () key == Set.singleton key
  expect "placeholder has no dependencies" $
    Set.null (dependencies () key)

-- | NoOpTask semantics via ListTaskKey: no outputs, one dependency edge per
-- element.
testNoOpTaskKeySemantics :: IO ()
testNoOpTaskKeySemantics = do
  let listKey = listTaskKey [MkPKey "a", MkPKey "b"]
  expect "list task has no outputs" $
    Set.null (outKeys () listKey)
  expect "list task depends on all its elements" $
    Set.size (dependencies () listKey) == 2

-- | placeholdersOfType finds placeholders via taskPlaceholderKey.
testPlaceholdersOfType :: IO ()
testPlaceholdersOfType = do
  let
    a = testTask "A" [p1] [q]
    taskMap = Map.fromList [(a, Set.empty)]
  -- TestTask never implements taskPlaceholderKey, so nothing is found; the
  -- real Task r k implementation is exercised in the blocks-3d tests.
  expect "placeholdersOfType finds nothing among non-placeholders" $
    null (placeholdersOfType @TestTask @PKey taskMap)
  expect "taskPlaceholderKey defaults to Nothing" $
    isNothing (taskPlaceholderKey @TestTask @PKey a)
  expect "taskInputPaths of test task" $
    taskInputPaths a == Set.singleton (VirtualFilePath p1)

-- | A task that has something to run remotely but allows itself no CPUs would
-- throw when it ran, for want of a worker, so the map is rejected instead. The
-- minimum is a floor rather than the allocation, so such a task need not fail
-- every time -- which is why this is not left to a warning.
testComputeTasksHaveCpus :: IO ()
testComputeTasksHaveCpus = do
  let computing = (testTask "A" [] [q]) { computes = True }
  expectInvalid "a computing task that allows itself no CPUs is invalid" $
    taskMapOf computing { minThreads = 0 }
  expectValid "a computing task that asks for a CPU is valid" $
    taskMapOf computing
  -- A no-op has no closure and is expected to ask for no CPUs.
  expectValid "a task with nothing to run may ask for no CPUs" $
    taskMapOf (testTask "B" [] [r]) { minThreads = 0 }

-- | Instrumentation gaps are reported for tasks that compute, and only those:
-- declaring nothing is correct for a task that performs no computation.
testInstrumentationGaps :: IO ()
testInstrumentationGaps = do
  let
    gapsOf t = Set.fromList $ Map.keys $ taskInstrumentationGaps (taskMapOf t)
  -- 'mkFileInfo' declares a size of zero, so this task is reported on both
  -- axes: no stat key, and no declared size for the file it produces.
  expect "a computing task that declares nothing is reported on both axes" $
    gapsOf ((testTask "A" [] [q]) { computes = True })
      == Set.fromList [NoStatKey, ZeroFileSizeEstimate]
  expect "a task that computes nothing is not reported" $
    Set.null (gapsOf (testTask "A" [] [q]))

-- | A maximum below the minimum can be satisfied by no allocation, and the
-- allocator resolves it by capping at the maximum -- handing the task fewer
-- threads than it asked for, or none at all.
testThreadRangesAreOrdered :: IO ()
testThreadRangesAreOrdered = do
  let computing = (testTask "A" [] [q]) { computes = True }
  expectInvalid "maxThreads below minThreads is invalid" $
    taskMapOf computing { minThreads = 2, maxThreads = 1 }
  expectValid "maxThreads equal to minThreads is valid" $
    taskMapOf computing { minThreads = 2, maxThreads = 2 }

-- | A task declaring the size of each file, to tell whose file info a
-- consumer ends up with.
data SizedTask = MkSizedTask
  { sizedName    :: String
  , sizedInputs  :: [(OsPath, FileSize)]
  , sizedOutputs :: [(OsPath, FileSize)]
  , sumsInputs   :: Bool
    -- ^ Whether, given its inputs, each output's size is their total instead.
  } deriving (Eq, Ord, Show, Generic, ToJSON, Binary)

instance IsTask SizedTask where
  taskTag t     = Just (Text.pack t.sizedName)
  taskClosure _ = Nothing
  taskSummary knownInputs t = summary
    where
      summary = filesOnlySummary ownInputs outputs knownInputs
      ownInputs = Set.fromList $ map sizedInfo t.sizedInputs
      ownOutputs = Set.fromList $ map sizedInfo t.sizedOutputs
      outputs
        | t.sumsInputs = Set.map (\o -> o { fileSize = EstimatedByTask total }) ownOutputs
        | otherwise    = ownOutputs
      total = sum [ schedulingEstimate i.fileSize | i <- Set.toList summary.inputs ]

sizedTask :: String -> [(OsPath, FileSize)] -> [(OsPath, FileSize)] -> SizedTask
sizedTask name ins outs = MkSizedTask name ins outs False

sizedInfo :: (OsPath, FileSize) -> TaskKeyFileInfo
sizedInfo (path, size) = (mkFileInfo path) { fileSize = EstimatedByTask size }

-- | A filesystem holding files of the given sizes.
newtype Disk a = MkDisk (Map OsPath FileSize -> a)
  deriving newtype (Functor, Applicative, Monad)

instance MonadTaskFiles Disk where
  taskFileSize path = MkDisk (Map.lookup path)

runDisk :: Map OsPath FileSize -> Disk a -> a
runDisk files (MkDisk f) = f files

testInputSizes :: IO ()
testInputSizes = do
  let
    producer = wrapTask $ sizedTask "P" [] [(q, 7)]
    -- r is on disk, s is missing.
    consumer = wrapTask $ (sizedTask "C" [(q, 0), (r, 0), (s, 0)] [(p1, 1)]) { sumsInputs = True }
    root = wrapTask $ sizedTask "R" [(p1, 1)] []
    taskMap = Map.fromList
      [ (producer, Set.empty)
      , (consumer, Set.singleton producer)
      , (root, Set.singleton consumer)
      ]
    -- The size of q recorded in statistics.
    stats = MkTaskAndFileStats (MkTaskStats Map.empty) $ MkFileStats $
      Map.singleton (fileStatKeyOfPath q) $
        Map.singleton unitSummary (toTrials (NonEmpty.singleton 70))
    estimated = estimatedTasks $ runDisk (Map.fromList [(r, 3), (q, 1000)]) (mkEstimatedTaskMap stats taskMap)
    inputSizes t = [ (i.path, i.fileSize) | i <- Set.toList (taskInputs t) ]
    isConsumer = (== Just "C") . taskTag
    expected =
      [ (VirtualFilePath q, MeasuredFromStats 70 7)
      , (VirtualFilePath r, EstimatedByTask 3)
      , (VirtualFilePath s, EstimatedByTask 0)
      ]
  expect "an input takes the file info of the task producing it, after stats; \
         \another input the size on disk, 0 if missing" $
    map inputSizes (filter isConsumer (Map.keys estimated)) == [expected]
  expect "a dependency in the map's values has the same inputs" $
    map inputSizes (filter isConsumer (concatMap Set.toList (Map.elems estimated))) == [expected]
  expect "an output computed from the inputs is what its consumer sees" $
    map inputSizes (filter ((== Just "R") . taskTag) (Map.keys estimated))
      == [[(VirtualFilePath p1, EstimatedByTask 73)]]

-- | 'validateTaskMap' rejects cycles, but estimating the map first must not
-- hang on one.
testInputSizesInCycle :: IO ()
testInputSizesInCycle = do
  let
    a = wrapTask $ (sizedTask "A" [(q, 1)] [(r, 2)]) { sumsInputs = True }
    b = wrapTask $ (sizedTask "B" [(r, 3)] [(q, 4)]) { sumsInputs = True }
    estimated = estimatedTasks $ runDisk Map.empty $ mkEstimatedTaskMap mempty $ Map.fromList
      [ (a, Set.singleton b)
      , (b, Set.singleton a)
      ]
    sizes = [ schedulingEstimate i.fileSize | t <- Map.keys estimated, i <- Set.toList (taskInputs t) ]
  expect "estimating a cycle terminates" $ length sizes == 2 && sum sizes > 0

runTest :: IO ()
runTest = do
  testInputSizes
  testInputSizesInCycle
  testPrunedInputIsValid
  testMissingEdgeIsInvalid
  testCorrectEdgeIsValid
  testUnreplacedPlaceholderIsInvalid
  testReplaceTasksConnectsToRootsOnly
  testPlaceholderTaskKeySemantics
  testNoOpTaskKeySemantics
  testPlaceholdersOfType
  testComputeTasksHaveCpus
  testThreadRangesAreOrdered
  testInstrumentationGaps
  putStrLn "All TaskMap tests passed."
