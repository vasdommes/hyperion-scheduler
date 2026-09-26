{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Pure unit tests for estimate provenance: what 'decorateTaskWithStats'
-- records about where a figure came from, and what a task record says its
-- estimates were. Runs locally, no cluster or filesystem access needed.
module Hyperion.Scheduler.Test.EstimateTest where

import Control.Exception                   (AssertionFailed (..), throwIO)
import Control.Monad                       (unless)
import Data.Aeson                          (FromJSON, ToJSON)
import Data.Aeson                          qualified as Aeson
import Data.Binary                         (Binary)
import Data.List.NonEmpty                  qualified as NonEmpty
import Data.Map.Strict                     qualified as Map
import Data.Set                            qualified as Set
import Data.Text                           qualified as Text
import Data.Time                           (UTCTime (..), fromGregorian)
import GHC.Generics                        (Generic)
import Hyperion                            (WorkerAddr (..))
import Hyperion.OsString                   (fromString)
import Hyperion.Scheduler.FilePath         (VirtualFilePath (..))
import Hyperion.Scheduler.StatKey          (IsFileStatKey (..), IsStatKey (..),
                                            TaskKeyFileInfo (..),
                                            encodeFileStatKey, encodeStatKey)
import Hyperion.Scheduler.Stats            (Accuracy (..), TaskAndFileStats,
                                            TaskEstimates (..), TaskRecord (..),
                                            Trials (..), estimateAccuracy,
                                            fileSizeAccuracy, modelAccuracy,
                                            recordToTaskStats, taskEstimatesAt,
                                            toTrials)
import Hyperion.Scheduler.Task.IsTask      (IsTask (..), ResourceEstimates (..),
                                            estimatesFromModel,
                                            taskRuntimeEstimate)
import Hyperion.Scheduler.Task.TaskMap     (TaskMap, statsCoverage,
                                            underestimatedMemoryTags)
import Hyperion.Scheduler.Task.WrappedTask (WrappedTask, decorateTaskWithStats,
                                            wrapTask)
import Hyperion.Scheduler.Types            (Estimate (..), FileSize,
                                            MemorySize (..), Node (..), NumCPUs,
                                            defaultRuntimeEstimate,
                                            isMeasuredFromStats, modelEstimate,
                                            schedulingEstimate)

-- * A minimal task with a stat key

-- | Reduced to the task's name: two tasks of the same name share statistics.
newtype EstStatKey = MkEstStatKey String
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

-- | The task's own memory model. 'MkEstTask' overrides it, so that a test can
-- make the model and the measurement disagree.
instance IsStatKey EstStatKey where
  memoryEstimate _ = 1024 * 1024

newtype EstFileStatKey = MkEstFileStatKey String
  deriving newtype (ToJSON)

instance IsFileStatKey EstFileStatKey where
  fileSizeEstimate _ = 100

data EstTask = MkEstTask
  { name   :: String
  , memory :: MemorySize
  } deriving (Eq, Ord, Show, Generic, ToJSON)

instance Binary EstTask

instance IsTask EstTask where
  taskResourceEstimates t = estimatesFromModel t.memory
  taskInputs _ = Set.empty
  taskOutputs t = Set.singleton MkTaskKeyFileInfo
    { fileStatKey = Just $ encodeFileStatKey (MkEstFileStatKey t.name)
    , path        = VirtualFilePath $ fromString ("/data/" <> t.name)
    , fileSize    = EstimatedByTask $ fileSizeEstimate (MkEstFileStatKey t.name)
    }
  taskTag t = Just (Text.pack t.name)
  taskClosure _ _ = Nothing
  taskStatKey t = Just $ encodeStatKey (MkEstStatKey t.name)

estTask :: String -> MemorySize -> WrappedTask
estTask name memory = wrapTask MkEstTask { name = name, memory = memory }

-- | A task that reports estimates already measured from statistics, as a
-- 'WrappedTask' does once decorated.
data PreMeasuredTask = MkPreMeasuredTask
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance Binary PreMeasuredTask

instance IsTask PreMeasuredTask where
  taskInputs _ = Set.empty
  taskOutputs _ = Set.empty
  taskTag _ = Just "PreMeasured"
  taskClosure _ _ = Nothing
  taskResourceEstimates _ = MkResourceEstimates
    { memory  = MeasuredFromStats (8 * 1024 * 1024) (1024 * 1024)
    , runtime = MeasuredFromStats (const 10) (const 1)
    }

-- * Statistics built from records

testNode :: Node
testNode = MkNode
  { memory           = 64 * 1024 * 1024 * 1024
  , cpus             = 8
  , localStoragePath = fromString "/tmp"
  , localStorageSize = 1024 * 1024
  , address          = LocalHost (fromString "localhost")
  }

-- | A record of a task having run, as 'runTasks' would have written it. The
-- estimates it carries are the ones the task was scheduled on.
estRecord :: String -> NumCPUs -> Maybe MemorySize -> FileSize -> TaskRecord EstTask
estRecord name numCpus memory fileSize = MkTaskRecord
  { task          = task
  , taskStart     = UTCTime (fromGregorian 2026 1 1) 0
  , taskRuntime   = 100
  , taskMemory    = memory
  , taskNode      = testNode
  , taskNumCPUs   = numCpus
  , taskFileSizes = Map.singleton fileStatKey (NonEmpty.singleton fileSize)
  , taskEstimates = taskEstimatesAt numCpus task
  , taskStatKey   = taskStatKey task
  }
  where
    task = MkEstTask { name = name, memory = 1024 * 1024 }
    fileStatKey = encodeFileStatKey (MkEstFileStatKey name)

statsOf :: [TaskRecord EstTask] -> TaskAndFileStats
statsOf = foldMap recordToTaskStats

-- * Assertions

expect :: String -> Bool -> IO ()
expect label cond = do
  unless cond $ throwIO $ AssertionFailed $ "FAILED: " <> label
  putStrLn $ "ok: " <> label

-- * Tests

-- | With memory and runtime both recorded, both estimates are replaced, and
-- each keeps the task's own prediction for comparison.
testStatsOverrideKeepsOwnEstimate :: IO ()
testStatsOverrideKeepsOwnEstimate = do
  let
    ownMemory = 1024 * 1024
    stats = statsOf [estRecord "A" 4 (Just (8 * 1024 * 1024)) 4096]
    estimates = taskResourceEstimates $
      decorateTaskWithStats stats (estTask "A" ownMemory)
  expect "measured memory is used" $
    schedulingEstimate estimates.memory == 8 * 1024 * 1024
  expect "the task's own memory model is kept" $
    modelEstimate estimates.memory == ownMemory
  expect "measured runtime is used" $
    schedulingEstimate estimates.runtime 4 /= modelEstimate estimates.runtime 4
  expect "the task's own runtime model is kept" $
    modelEstimate estimates.runtime 4 == defaultRuntimeEstimate ownMemory 4

-- | A record with no memory figure still yields runtime statistics. Provenance
-- is per quantity, so the runtime is measured while the memory estimate stays
-- the task's own.
testRuntimeMeasuredWithoutMemory :: IO ()
testRuntimeMeasuredWithoutMemory = do
  let
    ownMemory = 1024 * 1024
    stats = statsOf [estRecord "A" 4 Nothing 4096]
    task = decorateTaskWithStats stats (estTask "A" ownMemory)
    estimates = taskResourceEstimates task
  expect "memory with no statistics stays the task's own" $
    case estimates.memory of
      EstimatedByTask m -> m == ownMemory
      _                 -> False
  expect "runtime is measured even with no memory statistics" $
    case estimates.runtime of
      MeasuredFromStats _ _ -> True
      _                     -> False
  expect "a runtime-only match counts towards stats coverage" $
    statsCoverage (taskMapOf [task]) == (1, 1)

-- | A stat key that matches nothing leaves both estimates alone.
testNoMatchKeepsTaskEstimates :: IO ()
testNoMatchKeepsTaskEstimates = do
  let
    stats = statsOf [estRecord "A" 4 (Just (8 * 1024 * 1024)) 4096]
    task = decorateTaskWithStats stats (estTask "B" (2 * 1024 * 1024))
    estimates = taskResourceEstimates task
  expect "unmatched memory stays the task's own" $
    case estimates.memory of
      EstimatedByTask m -> m == 2 * 1024 * 1024
      _                 -> False
  expect "unmatched runtime stays the task's own" $
    case estimates.runtime of
      EstimatedByTask _ -> True
      _                 -> False
  expect "an unmatched task does not count towards stats coverage" $
    statsCoverage (taskMapOf [task]) == (0, 1)

-- | Decorating twice must not mistake the first measurement for the task's
-- own prediction.
testDecorateIsIdempotent :: IO ()
testDecorateIsIdempotent = do
  let
    ownMemory = 1024 * 1024
    stats = statsOf [estRecord "A" 4 (Just (8 * 1024 * 1024)) 4096]
    decorate = decorateTaskWithStats stats
    estimates = taskResourceEstimates $ decorate $ decorate (estTask "A" ownMemory)
  expect "decorating twice keeps the task's own memory model" $
    modelEstimate estimates.memory == ownMemory
  expect "decorating twice keeps the measured memory" $
    schedulingEstimate estimates.memory == 8 * 1024 * 1024

-- | A task whose measured memory dwarfs its own model is reported.
testUnderestimatedMemoryIsReported :: IO ()
testUnderestimatedMemoryIsReported = do
  let
    stats = statsOf [estRecord "A" 4 (Just (8 * 1024 * 1024)) 4096]
    decorate = decorateTaskWithStats stats
    overOptimistic = decorate (estTask "A" (1024 * 1024))
    accurate       = decorate (estTask "A" (7 * 1024 * 1024))
  expect "a model 8x below the measurement is reported" $
    underestimatedMemoryTags 2 (taskMapOf [overOptimistic]) == Set.singleton (Just "A")
  expect "a model close to the measurement is not reported" $
    Set.null (underestimatedMemoryTags 2 (taskMapOf [accurate]))

-- | 'wrapTask' must not relabel a measurement as the task's own prediction.
-- No task in this repository reports measurements of its own -- only
-- 'decorateTaskWithStats' does, and a 'WrappedTask' cannot be wrapped again for
-- want of a 'Binary' instance -- so this guards the class contract rather than
-- a path that exists today.
testWrapKeepsMeasuredEstimates :: IO ()
testWrapKeepsMeasuredEstimates = do
  let
    task = wrapTask MkPreMeasuredTask
    estimates = taskResourceEstimates task
  expect "wrapping keeps a measured memory figure measured" $
    isMeasuredFromStats estimates.memory
  expect "wrapping keeps the measured memory" $
    schedulingEstimate estimates.memory == 8 * 1024 * 1024
  expect "wrapping keeps the task's own memory model" $
    modelEstimate estimates.memory == 1024 * 1024
  expect "wrapping keeps a measured runtime measured" $
    isMeasuredFromStats estimates.runtime

-- | Merging is the only arithmetic these statistics do, and it had no test.
-- Checked against the mean and biased variance computed directly from the same
-- observations.
testTrialsSummary :: IO ()
testTrialsSummary = do
  let
    xs = [1, 2, 3, 4, 10] :: [Double]
    trials = toTrials (NonEmpty.fromList xs)
    reversed = toTrials (NonEmpty.fromList (reverse xs))
    n = fromIntegral (length xs) :: Double
    expectedMean = sum xs / n
    expectedVariance = sum (map (^ (2 :: Int)) xs) / n - expectedMean * expectedMean
    close a b = abs (a - b) < 1e-9
  expect "mean of the observations" $ close trials.mean expectedMean
  expect "biased variance of the observations" $ close trials.variance expectedVariance
  expect "extremes and count of the observations" $
    (trials.min, trials.max, trials.numTrials) == (1, 10, 5)
  expect "the summary does not depend on merge order" $
    close reversed.mean trials.mean && close reversed.variance trials.variance

-- | A file's declared size survives being overridden by a recorded one, the
-- same way a task's memory model does.
testFileSizeKeepsOwnEstimate :: IO ()
testFileSizeKeepsOwnEstimate = do
  let
    -- The key declares 100 bytes (see 'EstFileStatKey'); the run measured 4096.
    stats = statsOf [estRecord "A" 4 (Just (8 * 1024 * 1024)) 4096]
    outputs = Set.toList $ taskOutputs $ decorateTaskWithStats stats (estTask "A" 1024)
  case outputs of
    [output] -> do
      expect "the recorded file size is used" $
        schedulingEstimate output.fileSize == 4096
      expect "the key's own file size estimate is kept" $
        modelEstimate output.fileSize == 100
    _ -> throwIO $ AssertionFailed "FAILED: expected exactly one output file"

-- | File sizes score like memory and runtime do: measured over predicted, and
-- against either estimate.
testFileSizeAccuracy :: IO ()
testFileSizeAccuracy = do
  let
    records = [estRecord "A" 4 (Just (8 * 1024 * 1024)) 4096]
    fileStatKey = encodeFileStatKey (MkEstFileStatKey "A")
    modelRatio = (.mean) <$> Map.lookup fileStatKey (fileSizeAccuracy modelEstimate records)
    scheduledRatio = (.mean) <$> Map.lookup fileStatKey (fileSizeAccuracy schedulingEstimate records)
  -- An undecorated record predicts the declared 100 bytes either way.
  expect "a file 41x bigger than declared scores 40.96" $
    modelRatio == Just 40.96
  expect "with no statistics, both file size accuracies agree" $
    scheduledRatio == modelRatio

-- | Accuracy is measured over predicted, so a model that predicts too little
-- scores above one. The records here were scheduled on the tasks' own models,
-- so both accuracies agree.
testAccuracyRatios :: IO ()
testAccuracyRatios = do
  let
    -- The model predicts 1 MiB (see 'estRecord'); the run measured 8 MiB.
    records = [estRecord "A" 4 (Just (8 * 1024 * 1024)) 4096]
    statKey = encodeStatKey (MkEstStatKey "A")
    memoryRatio accuracies = do
      accuracy <- Map.lookup statKey accuracies
      (.mean) <$> accuracy.memory
  expect "a model predicting an eighth of the memory scores 8" $
    memoryRatio (modelAccuracy records) == Just 8
  expect "with no statistics, scheduling accuracy equals model accuracy" $
    estimateAccuracy records == modelAccuracy records
  expect "a record with no memory figure is not scored for memory" $
    memoryRatio (modelAccuracy [estRecord "A" 4 Nothing 4096]) == Nothing

-- | A task record must survive the round trip through JSON: that is what turns
-- the records from a log into data that can be grouped and scored later. Only
-- the task itself comes back opaque, as a 'Aeson.Value' -- a 'WrappedTask' can
-- have no 'FromJSON'.
testRecordRoundTrip :: IO ()
testRecordRoundTrip = do
  let record = estRecord "A" 4 (Just (8 * 1024 * 1024)) 4096
  expect "a measured estimate round-trips" $
    Aeson.eitherDecode (Aeson.encode measured) == Right measured
  case Aeson.eitherDecode (Aeson.encode record) of
    Left err -> throwIO $ AssertionFailed $ "FAILED: record does not parse: " <> err
    Right (parsed :: TaskRecord Aeson.Value) -> do
      expect "the stat key survives the round trip" $
        parsed.taskStatKey == record.taskStatKey
      expect "the estimates survive the round trip" $
        parsed.taskEstimates == record.taskEstimates
      expect "the measurements survive the round trip" $
        (parsed.taskRuntime, parsed.taskMemory, parsed.taskNumCPUs, parsed.taskFileSizes)
          == (record.taskRuntime, record.taskMemory, record.taskNumCPUs, record.taskFileSizes)
      expect "statistics can be rebuilt from a parsed record" $
        recordToTaskStats parsed == recordToTaskStats record
  where
    measured :: Estimate MemorySize
    measured = MeasuredFromStats (8 * 1024 * 1024) (1024 * 1024)

-- | What a task record says about the estimates the task ran on.
testTaskEstimatesAt :: IO ()
testTaskEstimatesAt = do
  let
    ownMemory = 1024 * 1024
    task = estTask "A" ownMemory
    estimates = taskEstimatesAt 4 task
    fileStatKey = encodeFileStatKey (MkEstFileStatKey "A")
  expect "recorded runtime estimate is the scheduler's curve at the given CPUs" $
    schedulingEstimate estimates.runtime == taskRuntimeEstimate task 4
  expect "recorded memory estimate is the scheduler's figure" $
    schedulingEstimate estimates.memory == ownMemory
  expect "file size estimates are keyed as the recorded sizes are" $
    Map.lookup fileStatKey estimates.fileSizes == Just (EstimatedByTask 100)

taskMapOf :: [WrappedTask] -> TaskMap WrappedTask
taskMapOf tasks = Map.fromList [(t, Set.empty) | t <- tasks]

runTest :: IO ()
runTest = do
  testStatsOverrideKeepsOwnEstimate
  testRuntimeMeasuredWithoutMemory
  testNoMatchKeepsTaskEstimates
  testDecorateIsIdempotent
  testUnderestimatedMemoryIsReported
  testWrapKeepsMeasuredEstimates
  testTrialsSummary
  testFileSizeKeepsOwnEstimate
  testFileSizeAccuracy
  testAccuracyRatios
  testRecordRoundTrip
  testTaskEstimatesAt
  putStrLn "All estimate tests passed."
