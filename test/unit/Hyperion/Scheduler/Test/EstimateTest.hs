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
                                            encodeFileStatKey, encodeStatKey,
                                            unitSummary)
import Hyperion.Scheduler.Stats            (TaskAndFileStats,
                                            TaskEstimates (..), TaskRecord (..),
                                            Trials (..), lookupFileStats,
                                            recordToTaskStats, taskEstimatesAt,
                                            toTrials)
import Hyperion.Scheduler.Task.IsTask      (IsTask (..), ResourceEstimates (..),
                                            TaskSummary (..),
                                            estimatesFromModel,
                                            filesOnlySummary, taskOutputs,
                                            taskResourceEstimates,
                                            taskRuntimeEstimate, taskStatKey)
import Hyperion.Scheduler.Task.TaskMap     (TaskMap, underestimatedMemoryTags)
import Hyperion.Scheduler.Task.WrappedTask (WrappedTask, decorateTaskWithStats,
                                            wrapTask)
import Hyperion.Scheduler.Types            (Estimate (..), FileSize,
                                            MemorySize (..), Node (..), NumCPUs,
                                            defaultRuntimeEstimate, isFromStats,
                                            modelEstimate, schedulingEstimate)

-- * A minimal task with a stat key

-- | Reduced to the task's name: two tasks of the same name share statistics.
newtype EstStatKey = MkEstStatKey String
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

-- | The task's own memory model. 'MkEstTask' overrides it, so that a test can
-- make the model and the measurement disagree.
instance IsStatKey EstStatKey where
  memoryEstimate _ _ = 1024 * 1024

newtype EstFileStatKey = MkEstFileStatKey String
  deriving newtype (ToJSON)

instance IsFileStatKey EstFileStatKey where
  fileSizeEstimate _ _ = 100

data EstTask = MkEstTask
  { name   :: String
  , memory :: MemorySize
  } deriving (Eq, Ord, Show, Generic, ToJSON)

instance Binary EstTask

instance IsTask EstTask where
  taskSummary _ t = MkTaskSummary
    { inputs      = Set.empty
    , outputs     = Set.singleton MkTaskKeyFileInfo
      { fileStatKey = Just $ encodeFileStatKey (MkEstFileStatKey t.name)
      , path        = VirtualFilePath $ fromString ("/data/" <> t.name)
      , fileSize    = EstimatedByTask $ fileSizeEstimate (MkEstFileStatKey t.name) ()
      }
    , statKey     = Just $ encodeStatKey (MkEstStatKey t.name)
    , estimates   = estimatesFromModel t.memory
    , inputSummary = Just unitSummary
    , closeToInputSummary = const False
    , model       = const Nothing
    , producerSummary = Just unitSummary
    , closeToProducerSummary = \_ _ -> False
    , outputModel = const Nothing
    }
  taskTag t = Just (Text.pack t.name)
  taskClosure _ = Nothing

estTask :: String -> MemorySize -> WrappedTask
estTask name memory = wrapTask MkEstTask { name = name, memory = memory }

-- | A task that reports estimates already measured from statistics, as a
-- 'WrappedTask' does once decorated.
data PreMeasuredTask = MkPreMeasuredTask
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance Binary PreMeasuredTask

instance IsTask PreMeasuredTask where
  taskSummary knownInputs _ = (filesOnlySummary Set.empty Set.empty knownInputs)
    { estimates = MkResourceEstimates
      { memory  = MeasuredFromStats (8 * 1024 * 1024) (1024 * 1024)
      , runtime = MeasuredFromStats (const 10) (const 1)
      }
    }
  taskTag _ = Just "PreMeasured"
  taskClosure _ = Nothing

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
  , taskInputSummary = Just unitSummary
  , taskProducerSummary = Just unitSummary
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
    isFromStats estimates.memory
  expect "wrapping keeps the measured memory" $
    schedulingEstimate estimates.memory == 8 * 1024 * 1024
  expect "wrapping keeps the task's own memory model" $
    modelEstimate estimates.memory == 1024 * 1024
  expect "wrapping keeps a measured runtime measured" $
    isFromStats estimates.runtime

-- | A task that ran on no CPUs contributes no resource statistics: its runtime
-- measured scheduler bookkeeping, and the sample could not be fitted anyway --
-- the speedup curve interpolates towards the origin, so a point at zero CPUs
-- divides by zero and yields NaN and infinity for every CPU count.
testZeroCpuRecordContributesNothing :: IO ()
testZeroCpuRecordContributesNothing = do
  let
    stats = statsOf [estRecord "A" 0 (Just (8 * 1024 * 1024)) 4096]
    estimates = taskResourceEstimates $ decorateTaskWithStats stats (estTask "A" 1024)
    finite n = not (isNaN t) && not (isInfinite t)
      where t = realToFrac (schedulingEstimate estimates.runtime n) :: Double
  expect "a run on no CPUs leaves memory unmeasured" $
    not (isFromStats estimates.memory)
  expect "a run on no CPUs leaves the runtime curve unmeasured" $
    not (isFromStats estimates.runtime)
  expect "the runtime estimate stays finite at every CPU count" $
    all finite [0, 1, 4]
  -- File sizes are a property of the file, so they are recorded regardless.
  expect "a run on no CPUs still records its file sizes" $
    not (Map.null (lookupFileStats (encodeFileStatKey (MkEstFileStatKey "A")) stats))

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

-- | A task record must survive the round trip through JSON: that is what turns
-- the records from a log into data that can be grouped later. Only
-- the task itself comes back opaque, as a 'Aeson.Value' -- a 'WrappedTask' can
-- have no 'FromJSON'.
testRecordRoundTrip :: IO ()
testRecordRoundTrip = do
  let record = estRecord "A" 4 (Just (8 * 1024 * 1024)) 4096
  expect "a measured estimate round-trips" $
    Aeson.eitherDecode (Aeson.encode measured) == Right measured
  expect "a corrected estimate round-trips" $
    Aeson.eitherDecode (Aeson.encode corrected) == Right corrected
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
    measured, corrected :: Estimate MemorySize
    measured = MeasuredFromStats (8 * 1024 * 1024) (1024 * 1024)
    corrected = CorrectedByStats 1.5 (3 * 512 * 1024) (1024 * 1024)

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
  testZeroCpuRecordContributesNothing
  testTrialsSummary
  testFileSizeKeepsOwnEstimate
  testRecordRoundTrip
  testTaskEstimatesAt
  putStrLn "All estimate tests passed."
