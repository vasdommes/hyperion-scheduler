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

import Control.Exception               (AssertionFailed (..), evaluate, throwIO)
import Control.Monad                   (unless)
import Data.Aeson                      (FromJSON, ToJSON)
import Data.Aeson                      qualified as Aeson
import Data.Binary                     (Binary)
import Data.Foldable                   (traverse_)
import Data.IORef                      (IORef, readIORef)
import Data.Map.Strict                 qualified as Map
import Data.Maybe                      (isJust)
import Data.Set                        qualified as Set
import Hyperion.OsPath                 (OsPath)
import Hyperion.OsString               (fromString)
import Hyperion.Scheduler.FilePath     (VirtualFilePath (..))
import Hyperion.Scheduler.PathResolver (PathResolver (..))
import Hyperion.Scheduler.StatKey      (FileStatKey (..), FromInputFiles (..),
                                        InputFile (..), InputFileSizes (..),
                                        IsFileStatKey (..), IsStatKey (..),
                                        KeyedInputFileSizes (..),
                                        MaxInputFileSize (..),
                                        SizedTaskFile (..), TaskFile (..),
                                        ToFileStatKey (..),
                                        TotalInputFileSize (..))
import Hyperion.Scheduler.Task.IsTask  (InputInfos, ResourceEstimates (..),
                                        TaskEstimation (..), TaskShape (..))
import Hyperion.Scheduler.Task.Task    (DepKeys, Task (..), TaskKey (..),
                                        TaskKind (..), getPath, taskShapeOf)
import Hyperion.Scheduler.Test.Counted (counted, newCounter, resetCounter)
import Hyperion.Scheduler.Types        (Estimate (..), FileSize)
import System.IO.Unsafe                (unsafePerformIO)

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
  memoryEstimate _ (MkTotalInputFileSize total) = 2 * fromIntegral total

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
-- building a shape.
statKeyProjections, pathResolutions :: IORef Int
statKeyProjections = unsafePerformIO newCounter
{-# NOINLINE statKeyProjections #-}
pathResolutions = unsafePerformIO newCounter
{-# NOINLINE pathResolutions #-}

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
  expect "each estimation has its own estimates and files" $
    map (.estimates.memory) estimations
      == [EstimatedByTask 80, EstimatedByTask 120]
      && all ((== [VirtualFilePath sumPath]) . outputPaths) estimations
  statKeys <- readIORef statKeyProjections
  paths <- readIORef pathResolutions
  expect "the stat key is projected once" $ statKeys == 1
  expect "each path is resolved once" $ paths == 3

runTest :: IO ()
runTest = do
  testStockSummaries
  testEstimate
  testShapeIsBuiltOnce
  putStrLn "All InputSummary tests passed."
