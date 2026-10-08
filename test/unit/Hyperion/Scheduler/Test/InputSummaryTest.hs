{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Pure unit tests for estimates that depend on the sizes of a task's input
-- files ('taskShapeOf'). No cluster or filesystem access needed.
module Hyperion.Scheduler.Test.InputSummaryTest where

import Control.Exception                     (AssertionFailed (..), evaluate,
                                              throwIO)
import Control.Monad                         (unless)
import Data.Aeson                            (FromJSON, ToJSON)
import Data.Aeson                            qualified as Aeson
import Data.Binary                           (Binary)
import Data.Foldable                         (traverse_)
import Data.IORef                            (IORef, readIORef)
import Data.List.NonEmpty                    qualified as NonEmpty
import Data.Map.Strict                       qualified as Map
import Data.Maybe                            (isJust)
import Data.Set                              qualified as Set
import Data.Time                             (UTCTime (..), fromGregorian)
import Data.Time.Clock                       (NominalDiffTime)
import Hyperion                              (WorkerAddr (..))
import Hyperion.OsPath                       (OsPath)
import Hyperion.OsString                     (fromString)
import Hyperion.Scheduler.FilePath           (VirtualFilePath (..))
import Hyperion.Scheduler.PathResolver       (PathResolver (..))
import Hyperion.Scheduler.StatKey            (EncodedSummary (..),
                                              FileStatKey (..),
                                              FromInputFiles (..),
                                              InputFile (..),
                                              InputFileSizes (..),
                                              IsFileStatKey (..),
                                              IsStatKey (..), IsSummary (..),
                                              KeyedInputFileSizes (..),
                                              MaxInputFileSize (..),
                                              SizedTaskFile (..), TaskFile (..),
                                              ToFileStatKey (..),
                                              TotalInputFileSize (..),
                                              encodeFileStatKey, encodeStatKey,
                                              encodeSummary, nearSummaries)
import Hyperion.Scheduler.Stats              (ScheduledEstimates (..),
                                              TaskAndFileStats, TaskRecord (..),
                                              recordToStats)
import Hyperion.Scheduler.Task.EstimatedTask (applyStats, prepareStats)
import Hyperion.Scheduler.Task.IsTask        (InputInfos, Model (..),
                                              ResourceEstimates (..),
                                              TaskEstimation (..),
                                              TaskShape (..))
import Hyperion.Scheduler.Task.Task          (DepKeys, Task (..), TaskKey (..),
                                              TaskKind (..), getPath,
                                              taskShapeOf)
import Hyperion.Scheduler.Test.Counted       (counted, newCounter, resetCounter)
import Hyperion.Scheduler.Types              (Estimate (..), FileSize,
                                              MemorySize, Node (..),
                                              defaultRuntimeEstimate,
                                              modelEstimate, schedulingEstimate)
import System.IO.Unsafe                      (unsafePerformIO)

-- | An input file, produced elsewhere.
newtype LeafKey = MkLeafKey Int
  deriving newtype (Eq, Ord, Show, Binary, ToJSON)

type instance DepKeys LeafKey = '[]

instance TaskKey LeafKey where
  taskKind = PlaceholderTask

instance ToFileStatKey LeafKey

-- | Sums the given leaves.
newtype SumKey = MkSumKey [Int]
  deriving newtype (Eq, Ord, Show, Binary, ToJSON)

type instance DepKeys SumKey = '[LeafKey]

data SumStatKey = MkSumStatKey
  deriving (Eq, Ord, Show)

instance ToJSON SumStatKey where
  toJSON _ = Aeson.Null

instance FromJSON SumStatKey where
  parseJSON _ = pure MkSumStatKey

-- | Memory is twice the total input.
instance IsStatKey SumStatKey where
  type InputSummary SumStatKey = TotalInputFileSize
  memoryEstimate _ s@(MkTotalInputFileSize total) =
    counted modelEvaluations s (2 * fromIntegral total)

-- | The output is one byte more than the largest input.
instance IsFileStatKey SumStatKey where
  type ProducerSummary SumStatKey = MaxInputFileSize
  fileSizeEstimate _ (MkMaxInputFileSize largest) = largest + 1

instance ToFileStatKey SumKey where
  type FileStatKeyOf SumKey = SumStatKey
  fileStatKeyOf _ = Just MkSumStatKey

instance TaskKey SumKey where
  type StatKeyOf SumKey = SumStatKey
  toStatKey _ key = counted statKeyProjections key (Just MkSumStatKey)
  taskKind = CustomTask $ \_ _ key@(MkSumKey leaves) ->
    getPath key *> traverse_ (getPath . MkLeafKey) leaves *> pure (pure ())

data TestResolver = MkTestResolver

instance PathResolver TestResolver LeafKey where
  resolvePath _ (MkLeafKey i) =
    counted pathResolutions i (fromString ("/leaf/" <> show i))

instance PathResolver TestResolver SumKey where
  resolvePath _ key = counted pathResolutions key sumPath

-- | Calls of 'toStatKey' and 'resolvePath' above: the expensive work of
-- building a shape. And evaluations of the memory model.
statKeyProjections, pathResolutions, modelEvaluations :: IORef Int
statKeyProjections = unsafePerformIO newCounter
{-# NOINLINE statKeyProjections #-}
pathResolutions = unsafePerformIO newCounter
{-# NOINLINE pathResolutions #-}
modelEvaluations = unsafePerformIO newCounter
{-# NOINLINE modelEvaluations #-}

sumPath :: OsPath
sumPath = fromString "/sum"

leafPath :: Int -> VirtualFilePath
leafPath i = VirtualFilePath (fromString ("/leaf/" <> show i))

expect :: String -> Bool -> IO ()
expect label cond = do
  unless cond $ throwIO $ AssertionFailed $ "FAILED: " <> label
  putStrLn $ "ok: " <> label

-- | The stock summaries, built one file at a time.
testStockSummaries :: IO ()
testStockSummaries = do
  let
    keyA = MkFileStatKey (Aeson.String "a")
    keyB = MkFileStatKey (Aeson.String "b")
    files =
      [ MkInputFile { fileStatKey = Nothing, size = 30 }
      , MkInputFile { fileStatKey = Just keyB, size = 10 }
      , MkInputFile { fileStatKey = Just keyA, size = 20 }
      ]
    summarize :: FromInputFiles s => [InputFile] -> s
    summarize = foldMap fromInputFile
  expect "the size of the largest file" $
    summarize files == MkMaxInputFileSize 30
  expect "the total size" $
    summarize files == MkTotalInputFileSize 60
  expect "the sizes, sorted" $
    summarize files == MkInputFileSizes [10, 20, 30]
  expect "the keys and sizes, sorted" $
    summarize files
      == MkKeyedInputFileSizes [(Nothing, 30), (Just keyA, 20), (Just keyB, 10)]
  expect "two summaries of the same files" $
    summarize files == (MkMaxInputFileSize 30, MkTotalInputFileSize 60)
  expect "no files" $
    summarize [] == (MkMaxInputFileSize 0, MkTotalInputFileSize 0)

testEstimate :: IO ()
testEstimate = do
  let
    known file = Map.fromList
      [ (leafPath 1, info 1 (EstimatedByTask 10))
      , (leafPath 2, info 2 (MeasuredFromStats 30 20))
      , (leafPath 3, info 3 (EstimatedByTask 0))
      ] Map.! file.path
    info i size = MkSizedTaskFile
      { path = leafPath i, fileStatKey = Nothing, fileSize = size }
    shape = taskShapeOf MkTask
      { resolver = MkTestResolver, config = (), key = MkSumKey [1, 2, 3] }
    estimation = shape.estimate known
    outputSizes = [ (o.path, o.fileSize) | o <- Set.toList estimation.outputs ]
    memoryAt s = (.memory) <$> (shape.model >>= modelAt s)
  expect "inputs take the given infos" $
    [ (i.path, i.fileSize) | i <- Set.toList estimation.inputs ] ==
      [ (leafPath 1, EstimatedByTask 10)
      , (leafPath 2, MeasuredFromStats 30 20)
      , (leafPath 3, EstimatedByTask 0)
      ]
  expect "memory is the model of the input summary, from scheduled sizes" $
    estimation.estimates.memory == EstimatedByTask 80
  expect "an output size is the model of the producer input summary" $
    outputSizes == [(VirtualFilePath sumPath, EstimatedByTask 31)]
  expect "the model evaluates another summary" $
    memoryAt (encodeSummary (MkTotalInputFileSize 5))
      == Just (EstimatedByTask 10)
  expect "the model rejects a summary that does not decode" $
    memoryAt (MkEncodedSummary (Aeson.String "garbage")) == Nothing
  expect "the output model evaluates another summary" $
    (Map.lookup (VirtualFilePath sumPath) shape.outputModels
      >>= modelAt (encodeSummary (MkMaxInputFileSize 4)))
      == Just 5

-- | The model's figure at an encoded summary.
modelAt :: EncodedSummary -> Model r -> Maybe r
modelAt s (MkModel decode estimateAt _) = estimateAt <$> decode s

-- | A summary that cannot be measured, only told apart.
newtype Category = MkCategory String
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

instance IsSummary Category

testCloseness :: IO ()
testCloseness = do
  let
    size = MkTotalInputFileSize
    a = MkCategory "a"
  expect "sizes within a factor of 2 are close" $
    nearSummaries (size 1000) (size 1001) && nearSummaries (size 100) (size 150)
  expect "sizes 10x apart are not close" $
    not (nearSummaries (size 100) (size 1000))
  expect "sizes a factor of 2 apart are at distance 1, the edge of close" $
    summaryDistance (size 99) (size 199) == Just 1
      && nearSummaries (size 99) (size 199)
      && not (nearSummaries (size 99) (size 200))
  expect "a categorical summary is close only to itself" $
    nearSummaries (MkCategory "a") (MkCategory "a")
      && not (nearSummaries (MkCategory "a") (MkCategory "b"))
  expect "a pair is close when both parts are" $
    nearSummaries (a, size 100) (a, size 150)
      && not (nearSummaries (a, size 100) (MkCategory "b", size 100))
      && not (nearSummaries (a, size 100) (a, size 1000))

testNode :: Node
testNode = MkNode
  { memory           = 64 * 1024 * 1024 * 1024
  , cpus             = 8
  , localStoragePath = fromString "/tmp"
  , localStorageSize = 1024 * 1024
  , address          = LocalHost (fromString "localhost")
  }

-- | A run of a 'SumKey' task on 1 CPU, whose inputs had the given total and
-- largest size, measuring the given memory, runtime and output size.
sumRecord
  :: FileSize -> FileSize -> MemorySize -> NominalDiffTime -> FileSize
  -> TaskRecord ()
sumRecord total largest memory runtime outputSize = MkTaskRecord
  { task                = ()
  , taskStart           = UTCTime (fromGregorian 2026 1 1) 0
  , taskRuntime         = runtime
  , taskMemory          = Just memory
  , taskNode            = testNode
  , taskNumCPUs         = 1
  , taskFileSizes       = Map.singleton
      (encodeFileStatKey MkSumStatKey) (NonEmpty.singleton outputSize)
  , taskEstimates       = MkScheduledEstimates
      { memory    = EstimatedByTask 0
      , runtime   = EstimatedByTask 0
      , fileSizes = Map.empty
      }
  , taskStatKey         = Just (encodeStatKey MkSumStatKey)
  , taskInputSummary    = Just (encodeSummary (MkTotalInputFileSize total))
  , taskProducerSummary = Just (encodeSummary (MkMaxInputFileSize largest))
  }

-- | The estimation of a 'SumKey' task whose two inputs have the given
-- sizes, after statistics.
sumEstimation :: FileSize -> FileSize -> TaskEstimation
sumEstimation a b = applyStats (prepareStats sumStats [shape]) shape $
  shape.estimate (leafSizes a b)
  where
    shape = taskShapeOf MkTask
      { resolver = MkTestResolver, config = (), key = MkSumKey [1, 2] }

-- | Inputs totalling 100, at most 50. The model predicts memory 200 and output
-- 51; the run used 1.5x and wrote 2x that, and took 2x the model's runtime.
sumStats :: TaskAndFileStats
sumStats = recordToStats sumRun

-- | See 'sumStats'.
sumRun :: TaskRecord ()
sumRun = sumRecord 100 50 300 (2 * defaultRuntimeEstimate 200 1) 102

testCorrectionRules :: IO ()
testCorrectionRules = do
  let
    outputSize estimation = [ o.fileSize | o <- Set.toList estimation.outputs ]
    exact = sumEstimation 50 50
    close = sumEstimation 60 90
    far   = sumEstimation 500 500
    near a b = abs (a - b) <= 1e-9 * abs b
  expect "the same inputs use the measured memory" $
    exact.estimates.memory == MeasuredFromStats 300 200
  expect "the same inputs use the measured output size" $
    outputSize exact == [MeasuredFromStats 102 51]
  expect "close inputs correct the memory model by the largest ratio" $
    close.estimates.memory == CorrectedByStats 1.5 450 300
  expect "close inputs correct the runtime model by the mean ratio" $
    near (realToFrac (schedulingEstimate close.estimates.runtime 1) :: Double)
         (realToFrac (2 * modelEstimate close.estimates.runtime 1))
  expect "close inputs correct the output size model" $
    outputSize close == [CorrectedByStats 2 182 91]
  expect "far inputs keep the model" $
    far.estimates.memory == EstimatedByTask 2000
      && outputSize far == [EstimatedByTask 501]

-- | The two leaves of a 'SumKey' at the given sizes.
leafSizes :: FileSize -> FileSize -> InputInfos
leafSizes a b file = Map.fromList
  [ (leafPath i, leaf i size) | (i, size) <- [(1, a), (2, b)] ] Map.! file.path
  where
    leaf i size = MkSizedTaskFile
      { path        = leafPath i
      , fileStatKey = Nothing
      , fileSize    = EstimatedByTask size
      }

-- | 'estimate' reuses the work of building the shape: however many estimations
-- are made, the stat key is projected once and each path resolved once.
testShapeIsBuiltOnce :: IO ()
testShapeIsBuiltOnce = do
  resetCounter statKeyProjections
  resetCounter pathResolutions
  -- Bound at run time: a constant task would be floated out and shared with
  -- the other tests, and built before the counters are reset.
  leaves <- evaluate [1, 2]
  let
    shape = taskShapeOf MkTask
      { resolver = MkTestResolver, config = (), key = MkSumKey leaves }
    estimations =
      [ shape.estimate (leafSizes a b) | (a, b) <- [(10, 30), (20, 40)] ]
    outputPaths estimation = map (.path) (Set.toList estimation.outputs)
  expect "the shape has its stat key and files" $
    isJust shape.statKey
      && map (.path) (Set.toList shape.inputFiles) == [leafPath 1, leafPath 2]
      && map (.path) (Set.toList shape.outputFiles) == [VirtualFilePath sumPath]
  expect "each estimation has its own estimates, input summary and files" $
    map (.estimates.memory) estimations
      == [EstimatedByTask 80, EstimatedByTask 120]
      && all (isJust . (.inputSummary)) estimations
      && all ((== [VirtualFilePath sumPath]) . outputPaths) estimations
  statKeys <- readIORef statKeyProjections
  paths <- readIORef pathResolutions
  expect "the stat key is projected once" $ statKeys == 1
  expect "each path is resolved once" $ paths == 3

-- | 'prepareStats' decodes a stat key's recorded summaries, and evaluates the
-- model at them, once for all the tasks with that key.
testRecordedSummariesShared :: IO ()
testRecordedSummariesShared = do
  -- Bound at run time, so that nothing is built before the counter is reset.
  leaves <- evaluate [1, 2]
  resetCounter modelEvaluations
  let
    shapes =
      [ taskShapeOf MkTask
          { resolver = MkTestResolver, config = (), key = MkSumKey leaves }
      | _ <- [1 :: Int, 2]
      ]
    prepared = prepareStats sumStats shapes
    -- Inputs totalling 150 and 160, both close to the recorded 100.
    estimations =
      [ applyStats prepared shape (shape.estimate (leafSizes a a))
      | (shape, a) <- zip shapes [75, 80] ]
  expect "both tasks are corrected by the recorded statistics" $
    map (.estimates.memory) estimations
      == [CorrectedByStats 1.5 450 300, CorrectedByStats 1.5 480 320]
  evaluations <- readIORef modelEvaluations
  -- One for each task's own summary, one for the recorded summary.
  expect "the model is evaluated once at the recorded summary" $
    evaluations == 3

runTest :: IO ()
runTest = do
  testStockSummaries
  testEstimate
  testCloseness
  testCorrectionRules
  testShapeIsBuiltOnce
  testRecordedSummariesShared
  putStrLn "All InputSummary tests passed."
