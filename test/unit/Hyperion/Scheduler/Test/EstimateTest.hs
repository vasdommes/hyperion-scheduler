{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Pure unit tests for estimate provenance: what 'applyStats'
-- records about where a figure came from, and what a task record says its
-- estimates were. Runs locally, no cluster or filesystem access needed.
module Hyperion.Scheduler.Test.EstimateTest where

import Control.Exception                     (AssertionFailed (..), throwIO)
import Control.Monad                         (unless)
import Data.Aeson                            (FromJSON, ToJSON)
import Data.Aeson                            qualified as Aeson
import Data.Binary                           (Binary)
import Data.List.NonEmpty                    qualified as NonEmpty
import Data.Map.Strict                       qualified as Map
import Data.Maybe                            (isJust)
import Data.Set                              qualified as Set
import Data.Text                             qualified as Text
import Data.Time                             (UTCTime (..), fromGregorian)
import GHC.Generics                          (Generic)
import Hyperion                              (WorkerAddr (..))
import Hyperion.OsString                     (fromString)
import Hyperion.Scheduler.FilePath           (VirtualFilePath (..))
import Hyperion.Scheduler.StatKey            (IsFileStatKey (..),
                                              IsStatKey (..),
                                              SizedTaskFile (..), TaskFile (..),
                                              encodeFileStatKey, encodeStatKey,
                                              withSize)
import Hyperion.Scheduler.Stats              (ScheduledEstimates (..),
                                              TaskAndFileStats, TaskRecord (..),
                                              Trials (..), lookupMaxFileSize,
                                              recordToStats, taskEstimatesAt,
                                              toTrials)
import Hyperion.Scheduler.Task.EstimatedTask (EstimatedTask, applyStats,
                                              estimateTask, taskEstimation,
                                              taskOutputs,
                                              taskResourceEstimates,
                                              taskRuntimeEstimate)
import Hyperion.Scheduler.Task.IsTask        (InputInfos, IsTask (..),
                                              ResourceEstimates (..),
                                              TaskEstimation (..),
                                              TaskShape (..),
                                              estimatesFromModel,
                                              filesOnlyShape, taskStatKey)
import Hyperion.Scheduler.Task.WrappedTask   (WrappedTask, wrapTask)
import Hyperion.Scheduler.Types              (Estimate (..), FileSize,
                                              MemorySize (..), Node (..),
                                              NumCPUs, defaultRuntimeEstimate,
                                              isMeasuredFromStats,
                                              modelEstimate, schedulingEstimate)
import Hyperion.Scheduler.Util               (qualifiedTypeRepText)

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
  taskShape t = MkTaskShape
    { inputFiles  = Set.empty
    , outputFiles = Set.singleton output
    , statKey     = Just $ encodeStatKey (MkEstStatKey t.name)
    , estimate    = \_ -> MkTaskEstimation
        { inputs    = Set.empty
        , outputs   = Set.singleton $ withSize
            (EstimatedByTask (fileSizeEstimate (MkEstFileStatKey t.name)))
            output
        , estimates = estimatesFromModel t.memory
        }
    }
    where
      output = MkTaskFile
        { path        = VirtualFilePath $ fromString ("/data/" <> t.name)
        , fileStatKey = Just $ encodeFileStatKey (MkEstFileStatKey t.name)
        }
  taskTag t = Just (Text.pack t.name)
  taskClosure _ = Nothing

estTask :: String -> MemorySize -> WrappedTask
estTask name memory = wrapTask MkEstTask { name = name, memory = memory }

-- | The task estimated with the given statistics. None of these tasks has
-- inputs.
estimated :: IsTask a => TaskAndFileStats -> a -> EstimatedTask a
estimated stats = estimateTask stats noInputs

noInputs :: InputInfos
noInputs file = error $ "Unexpected input: " <> show file.path

-- | A task that reports estimates already measured from statistics, as
-- 'applyStats' leaves them.
data PreMeasuredTask = MkPreMeasuredTask
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance Binary PreMeasuredTask

instance IsTask PreMeasuredTask where
  taskShape _ = shape { estimate = \known -> (shape.estimate known)
    { estimates = MkResourceEstimates
      { memory  = MeasuredFromStats (8 * 1024 * 1024) (1024 * 1024)
      , runtime = MeasuredFromStats (const 10) (const 1)
      }
    } }
    where shape = filesOnlyShape Set.empty Set.empty
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
estRecord
  :: String -> NumCPUs -> Maybe MemorySize -> FileSize -> TaskRecord EstTask
estRecord name numCpus memory fileSize = MkTaskRecord
  { task          = task
  , taskStart     = UTCTime (fromGregorian 2026 1 1) 0
  , taskRuntime   = 100
  , taskMemory    = memory
  , taskNode      = testNode
  , taskNumCPUs   = numCpus
  , taskFileSizes = Map.singleton fileStatKey (NonEmpty.singleton fileSize)
  , taskEstimates =
      taskEstimatesAt numCpus (taskEstimation (estimated mempty task))
  , taskStatKey   = taskStatKey task
  }
  where
    task = MkEstTask { name = name, memory = 1024 * 1024 }
    fileStatKey = encodeFileStatKey (MkEstFileStatKey name)

statsOf :: [TaskRecord EstTask] -> TaskAndFileStats
statsOf = foldMap recordToStats

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
      estimated stats (estTask "A" ownMemory)
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
    task = estimated stats (estTask "A" ownMemory)
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
    task = estimated stats (estTask "B" (2 * 1024 * 1024))
    estimates = taskResourceEstimates task
  expect "unmatched memory stays the task's own" $
    case estimates.memory of
      EstimatedByTask m -> m == 2 * 1024 * 1024
      _                 -> False
  expect "unmatched runtime stays the task's own" $
    case estimates.runtime of
      EstimatedByTask _ -> True
      _                 -> False

-- | Applying statistics twice must not mistake the first measurement for the
-- task's own prediction.
testApplyStatsIsIdempotent :: IO ()
testApplyStatsIsIdempotent = do
  let
    ownMemory = 1024 * 1024
    stats = statsOf [estRecord "A" 4 (Just (8 * 1024 * 1024)) 4096]
    shape = taskShape (estTask "A" ownMemory)
    apply = applyStats stats shape
    estimates = (apply $ apply $ shape.estimate noInputs).estimates
  expect "applying statistics twice keeps the task's own memory model" $
    modelEstimate estimates.memory == ownMemory
  expect "applying statistics twice keeps the measured memory" $
    schedulingEstimate estimates.memory == 8 * 1024 * 1024

-- | Estimating must not relabel a measurement as the task's own prediction.
-- No task in this repository measures its own figures -- only
-- 'applyStats' does -- so this guards the class contract rather
-- than a path that exists today.
testEstimateKeepsMeasuredEstimates :: IO ()
testEstimateKeepsMeasuredEstimates = do
  let
    task = estimated mempty (wrapTask MkPreMeasuredTask)
    estimates = taskResourceEstimates task
  expect "estimating keeps a measured memory figure measured" $
    isMeasuredFromStats estimates.memory
  expect "estimating keeps the measured memory" $
    schedulingEstimate estimates.memory == 8 * 1024 * 1024
  expect "estimating keeps the task's own memory model" $
    modelEstimate estimates.memory == 1024 * 1024
  expect "wrapping keeps a measured runtime measured" $
    isMeasuredFromStats estimates.runtime

-- | A task that ran on no CPUs contributes no resource statistics: its runtime
-- measured scheduler bookkeeping, and the sample could not be fitted anyway --
-- the speedup curve interpolates towards the origin, so a point at zero CPUs
-- divides by zero and yields NaN and infinity for every CPU count.
testZeroCpuRecordContributesNothing :: IO ()
testZeroCpuRecordContributesNothing = do
  let
    stats = statsOf [estRecord "A" 0 (Just (8 * 1024 * 1024)) 4096]
    estimates = taskResourceEstimates $ estimated stats (estTask "A" 1024)
    fileStatKey = encodeFileStatKey (MkEstFileStatKey "A")
    finite n = not (isNaN t) && not (isInfinite t)
      where t = realToFrac (schedulingEstimate estimates.runtime n) :: Double
  expect "a run on no CPUs leaves memory unmeasured" $
    not (isMeasuredFromStats estimates.memory)
  expect "a run on no CPUs leaves the runtime curve unmeasured" $
    not (isMeasuredFromStats estimates.runtime)
  expect "the runtime estimate stays finite at every CPU count" $
    all finite [0, 1, 4]
  -- File sizes are a property of the file, so they are recorded regardless.
  expect "a run on no CPUs still records its file sizes" $
    isJust (lookupMaxFileSize fileStatKey stats)

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
    expectedVariance =
      sum (map (^ (2 :: Int)) xs) / n - expectedMean * expectedMean
    close a b = abs (a - b) < 1e-9
  expect "mean of the observations" $ close trials.mean expectedMean
  expect "biased variance of the observations" $
    close trials.variance expectedVariance
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
    outputs = Set.toList $ taskOutputs $ estimated stats (estTask "A" 1024)
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
  case Aeson.eitherDecode (Aeson.encode record) of
    Left err ->
      throwIO $ AssertionFailed $ "FAILED: record does not parse: " <> err
    Right (parsed :: TaskRecord Aeson.Value) -> do
      expect "the stat key survives the round trip" $
        parsed.taskStatKey == record.taskStatKey
      expect "the estimates survive the round trip" $
        parsed.taskEstimates == record.taskEstimates
      expect "the measurements survive the round trip" $
        (() <$ parsed) == (() <$ record)
      expect "statistics can be rebuilt from a parsed record" $
        recordToStats parsed == recordToStats record
  where
    measured :: Estimate MemorySize
    measured = MeasuredFromStats (8 * 1024 * 1024) (1024 * 1024)

-- | What a task record says about the estimates the task ran on.
testTaskEstimatesAt :: IO ()
testTaskEstimatesAt = do
  let
    ownMemory = 1024 * 1024
    task = estimated mempty (estTask "A" ownMemory)
    estimates = taskEstimatesAt 4 (taskEstimation task)
    fileStatKey = encodeFileStatKey (MkEstFileStatKey "A")
  expect "recorded runtime estimate is the scheduler's curve at given CPUs" $
    schedulingEstimate estimates.runtime == taskRuntimeEstimate task 4
  expect "recorded memory estimate is the scheduler's figure" $
    schedulingEstimate estimates.memory == ownMemory
  expect "file size estimates are keyed as the recorded sizes are" $
    Map.lookup fileStatKey estimates.fileSizes == Just (EstimatedByTask 100)

-- | Stat key types are told apart by this name, so that types of the same
-- name in different modules do not share statistics.
data TaggedKey = MkTaggedKey

testQualifiedTypeName :: IO ()
testQualifiedTypeName =
  expect "a type's qualified name has its module" $
    Text.unpack (qualifiedTypeRepText @TaggedKey)
      == "Hyperion.Scheduler.Test.EstimateTest.TaggedKey"

runTest :: IO ()
runTest = do
  testStatsOverrideKeepsOwnEstimate
  testRuntimeMeasuredWithoutMemory
  testNoMatchKeepsTaskEstimates
  testApplyStatsIsIdempotent
  testEstimateKeepsMeasuredEstimates
  testZeroCpuRecordContributesNothing
  testTrialsSummary
  testQualifiedTypeName
  testFileSizeKeepsOwnEstimate
  testRecordRoundTrip
  testTaskEstimatesAt
  putStrLn "All estimate tests passed."
