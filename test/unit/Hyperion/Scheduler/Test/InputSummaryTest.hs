{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Pure unit tests for estimates that depend on the sizes of a task's input
-- files ('summarizeTask'). No cluster or filesystem access needed.
module Hyperion.Scheduler.Test.InputSummaryTest where

import Control.Exception                   (AssertionFailed (..), throwIO)
import Control.Monad                       (unless)
import Data.Aeson                          (FromJSON, ToJSON)
import Data.Aeson                          qualified as Aeson
import Data.Binary                         (Binary)
import Data.Foldable                       (traverse_)
import Data.List.NonEmpty                  qualified as NonEmpty
import Data.Map.Strict                     qualified as Map
import Data.Set                            qualified as Set
import Data.Time                           (UTCTime (..), fromGregorian)
import Data.Time.Clock                     (NominalDiffTime)
import Hyperion                            (WorkerAddr (..))
import Hyperion.OsPath                     (OsPath)
import Hyperion.OsString                   (fromString)
import Hyperion.Scheduler.FilePath         (VirtualFilePath (..))
import Hyperion.Scheduler.PathResolver     (PathResolver (..))
import Hyperion.Scheduler.StatKey          (EncodedSummary (..),
                                            FileStatKey (..),
                                            FromInputFiles (..),
                                            InputFileSizes (..),
                                            InputFileSummary (..),
                                            IsFileStatKey (..), IsStatKey (..),
                                            IsSummary (..),
                                            KeyedInputFileSizes (..),
                                            MaxInputFileSize (..),
                                            TaskKeyFileInfo (..),
                                            ToFileStatKey (..),
                                            TotalInputFileSize (..),
                                            encodeFileStatKey, encodeStatKey,
                                            encodeSummary, nearSummaries)
import Hyperion.Scheduler.Stats            (Accuracy (..), TaskEstimates (..),
                                            TaskRecord (..), Trials (..),
                                            modelAccuracyOf,
                                            modelFileSizeAccuracyOf,
                                            recordToTaskStats)
import Hyperion.Scheduler.Task.IsTask      (ResourceEstimates (..),
                                            TaskSummary (..))
import Hyperion.Scheduler.Task.Task        (DepKeys, Task (..), TaskKey (..),
                                            TaskKind (..), getPath,
                                            summarizeTask)
import Hyperion.Scheduler.Task.WrappedTask (decorateSummaryWithStats)
import Hyperion.Scheduler.Types            (Estimate (..), FileSize, MemorySize,
                                            Node (..), defaultRuntimeEstimate,
                                            modelEstimate, schedulingEstimate)

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
  type ProducerInputSummary SumStatKey = MaxInputFileSize
  fileSizeEstimate _ (MkMaxInputFileSize largest) = largest + 1

instance ToFileStatKey SumKey where
  type FileStatKeyOf SumKey = SumStatKey
  fileStatKeyOf _ = Just MkSumStatKey

instance TaskKey SumKey where
  type StatKeyOf SumKey = SumStatKey
  toStatKey _ _ = Just MkSumStatKey
  taskKind = CustomTask $ \_ _ key@(MkSumKey leaves) ->
    getPath key *> traverse_ (getPath . MkLeafKey) leaves *> pure (pure ())

data TestResolver = MkTestResolver

instance PathResolver TestResolver LeafKey where
  resolvePath _ (MkLeafKey i) = fromString ("/leaf/" <> show i)

instance PathResolver TestResolver SumKey where
  resolvePath _ _ = sumPath

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
      [ MkInputFileSummary { fileStatKey = Nothing,   size = 30 }
      , MkInputFileSummary { fileStatKey = Just keyB, size = 10 }
      , MkInputFileSummary { fileStatKey = Just keyA, size = 20 }
      ]
    summarize :: FromInputFiles s => [InputFileSummary] -> s
    summarize = foldMap fromInputFile
  expect "the size of the largest file" $
    summarize files == MkMaxInputFileSize 30
  expect "the total size" $
    summarize files == MkTotalInputFileSize 60
  expect "the sizes, sorted" $
    summarize files == MkInputFileSizes [10, 20, 30]
  expect "the keys and sizes, sorted" $
    summarize files == MkKeyedInputFileSizes [(Nothing, 30), (Just keyA, 20), (Just keyB, 10)]
  expect "two summaries of the same files" $
    summarize files == (MkMaxInputFileSize 30, MkTotalInputFileSize 60)
  expect "no files" $
    summarize [] == (MkMaxInputFileSize 0, MkTotalInputFileSize 0)

testSummarizeTask :: IO ()
testSummarizeTask = do
  let
    known path = Map.lookup path $ Map.fromList
      [ (leafPath 1, info 1 (EstimatedByTask 10))
      , (leafPath 2, info 2 (MeasuredFromStats 30 20))
      ]
    info i size = MkTaskKeyFileInfo { path = leafPath i, fileStatKey = Nothing, fileSize = size }
    summary = summarizeTask (Just known) MkTask
      { resolver = MkTestResolver, config = (), key = MkSumKey [1, 2, 3] }
    outputSizes = [ (o.path, o.fileSize) | o <- Set.toList summary.outputs ]
  expect "inputs take the known infos, an unknown one the size 0" $
    [ (i.path, i.fileSize) | i <- Set.toList summary.inputs ] ==
      [ (leafPath 1, EstimatedByTask 10)
      , (leafPath 2, MeasuredFromStats 30 20)
      , (leafPath 3, EstimatedByTask 0)
      ]
  expect "memory is the model of the input summary, from scheduled sizes" $
    summary.estimates.memory == EstimatedByTask 80
  expect "an output size is the model of the producer input summary" $
    outputSizes == [(VirtualFilePath sumPath, EstimatedByTask 31)]
  expect "the model closure evaluates another summary" $
    fmap (.memory) (summary.model (encodeSummary (MkTotalInputFileSize 5)))
      == Just (EstimatedByTask 10)
  expect "the model closure rejects a summary that does not decode" $
    fmap (.memory) (summary.model (MkEncodedSummary (Aeson.String "garbage"))) == Nothing
  expect "the output model closure evaluates another summary" $
    summary.outputModel (encodeSummary (MkMaxInputFileSize 4))
      == Just (Map.singleton (VirtualFilePath sumPath) 5)

-- | A summary that cannot be measured, only told apart.
newtype Category = MkCategory String
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

instance IsSummary Category

testCloseness :: IO ()
testCloseness = do
  let size = MkTotalInputFileSize
  expect "sizes within a factor of 2 are close" $
    nearSummaries (size 1000) (size 1001) && nearSummaries (size 100) (size 150)
  expect "sizes 10x apart are not close" $
    not (nearSummaries (size 100) (size 1000))
  expect "a categorical summary is close only to itself" $
    nearSummaries (MkCategory "a") (MkCategory "a")
      && not (nearSummaries (MkCategory "a") (MkCategory "b"))
  expect "a pair is close when both parts are" $
    nearSummaries (MkCategory "a", size 100) (MkCategory "a", size 150)
      && not (nearSummaries (MkCategory "a", size 100) (MkCategory "b", size 100))
      && not (nearSummaries (MkCategory "a", size 100) (MkCategory "a", size 1000))

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
sumRecord :: FileSize -> FileSize -> MemorySize -> NominalDiffTime -> FileSize -> TaskRecord ()
sumRecord total largest memory runtime outputSize = MkTaskRecord
  { task                = ()
  , taskStart           = UTCTime (fromGregorian 2026 1 1) 0
  , taskRuntime         = runtime
  , taskMemory          = Just memory
  , taskNode            = testNode
  , taskNumCPUs         = 1
  , taskFileSizes       = Map.singleton (encodeFileStatKey MkSumStatKey) (NonEmpty.singleton outputSize)
  , taskEstimates       = MkTaskEstimates
      { memory = EstimatedByTask 0, runtime = EstimatedByTask 0, fileSizes = Map.empty }
  , taskStatKey         = Just (encodeStatKey MkSumStatKey)
  , taskInputSummary    = Just (encodeSummary (MkTotalInputFileSize total))
  , taskProducerSummary = Just (encodeSummary (MkMaxInputFileSize largest))
  }

-- | The estimated summary of a 'SumKey' task whose two inputs have the given
-- sizes, after statistics.
sumSummary :: FileSize -> FileSize -> TaskSummary
sumSummary a b = decorateSummaryWithStats stats $ summarizeTask (Just known) MkTask
  { resolver = MkTestResolver, config = (), key = MkSumKey [1, 2] }
  where
    known path = Map.lookup path $ Map.fromList
      [ (leafPath i, MkTaskKeyFileInfo { path = leafPath i, fileStatKey = Nothing, fileSize = EstimatedByTask size })
      | (i, size) <- [(1, a), (2, b)]
      ]
    -- Inputs totalling 100, at most 50. The model predicts memory 200 and
    -- output 51; the run used 1.5x and wrote 2x that, and took 2x the model's
    -- runtime.
    stats = recordToTaskStats $
      sumRecord 100 50 300 (2 * defaultRuntimeEstimate 200 1) 102

testCorrectionRules :: IO ()
testCorrectionRules = do
  let
    outputSize summary = [ o.fileSize | o <- Set.toList summary.outputs ]
    exact = sumSummary 50 50
    close = sumSummary 60 90
    far   = sumSummary 500 500
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
    far.estimates.memory == EstimatedByTask 2000 && outputSize far == [EstimatedByTask 501]

-- | The model can be judged against recorded runs offline: the records carry
-- the stat key and the summaries it is evaluated at.
testModelAccuracyOf :: IO ()
testModelAccuracyOf = do
  let
    records = [sumRecord 100 50 300 (2 * defaultRuntimeEstimate 200 1) 102]
    accuracy = Map.lookup (encodeStatKey MkSumStatKey) (modelAccuracyOf @SumStatKey records)
    fileAccuracy = Map.lookup (encodeFileStatKey MkSumStatKey) (modelFileSizeAccuracyOf @SumStatKey records)
  expect "memory is scored against the model at the recorded summary" $
    fmap (.mean) (accuracy >>= (.memory)) == Just 1.5
  expect "runtime is scored against the model at the recorded summary" $
    fmap (.mean) (accuracy >>= (.runtime)) == Just 2
  expect "output sizes are scored against the model at the recorded summary" $
    fmap (.mean) fileAccuracy == Just 2

runTest :: IO ()
runTest = do
  testStockSummaries
  testSummarizeTask
  testCloseness
  testCorrectionRules
  testModelAccuracyOf
  putStrLn "All InputSummary tests passed."
