{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}

-- | Tasks with their final estimations: their input sizes, and so their
-- estimates, are known. Only an estimated task has estimates, so they cannot
-- be read before 'estimateTask' or
-- 'Hyperion.Scheduler.Task.EstimatedTaskMap.mkEstimatedTaskMap'.
module Hyperion.Scheduler.Task.EstimatedTask
  ( EstimatedTask
  , originalTask
  , taskEstimation
  , PreparedStats
  , prepareStats
  , estimateTask
  , applyStats
  , taskInputs
  , taskOutputs
  , taskResourceEstimates
  , taskMemoryEstimate
  , taskRuntimeEstimate
  , taskMemoryCapped
  ) where

import Data.Aeson                     (ToJSON (..))
import Data.Map.Lazy                  qualified as LazyMap
import Data.Map.Strict                (Map)
import Data.Map.Strict                qualified as Map
import Data.Maybe                     (fromMaybe)
import Data.Set                       (Set)
import Data.Set                       qualified as Set
import Data.Time.Clock                (NominalDiffTime)
import Debug.Trace                    qualified as Debug
import Hyperion.Scheduler.StatKey     (EncodedSummary, FileStatKey,
                                       SizedTaskFile (..), StatKey,
                                       TaskFile (..))
import Hyperion.Scheduler.Stats       (TaskAndFileStats, TaskResourceMap,
                                       Trials (..), approxRuntime,
                                       fileSizeCorrection, lookupFileStats,
                                       lookupTaskStats, maxMemory,
                                       memoryCorrection, runtimeCorrection)
import Hyperion.Scheduler.Task.IsTask (InputInfos, IsTask (..), Model (..),
                                       ResourceEstimates (..),
                                       TaskEstimation (..), TaskShape (..))
import Hyperion.Scheduler.Types       (Estimate, FileSize, MemorySize, NumCPUs,
                                       correctWithStats, modelEstimate,
                                       overrideWithMeasured, schedulingEstimate)

-- | A task with its final estimation: estimates from the sizes of its input
-- files, after statistics.
data EstimatedTask a = MkEstimatedTask
  { task       :: a
  -- The task's shape, computed once here whatever the task type.
  , shape      :: TaskShape
  , estimation :: TaskEstimation
  }

originalTask :: EstimatedTask a -> a
originalTask t = t.task

instance Eq a => Eq (EstimatedTask a) where
  x == y = x.task == y.task

instance Ord a => Ord (EstimatedTask a) where
  compare x y = compare x.task y.task

instance ToJSON a => ToJSON (EstimatedTask a) where
  toJSON t = toJSON t.task

instance IsTask a => IsTask (EstimatedTask a) where
  taskShape t            = t.shape
  taskMaxThreads stage t = taskMaxThreads stage t.task
  taskMinThreads stage t = taskMinThreads stage t.task
  taskDefaultPriority t  = taskDefaultPriority t.task
  taskTag t              = taskTag t.task
  taskClosure t          = taskClosure t.task
  taskIsPlaceholder t    = taskIsPlaceholder t.task
  taskPlaceholderKey t   = taskPlaceholderKey t.task

-- | The task's final estimation.
taskEstimation :: EstimatedTask a -> TaskEstimation
taskEstimation t = t.estimation

-- | A task's final estimation, from the given infos of its input files, after
-- statistics.
estimateTask :: IsTask a => PreparedStats -> InputInfos -> a -> EstimatedTask a
estimateTask stats inputInfos t = MkEstimatedTask
  { task       = t
  , shape      = shape
  , estimation = applyStats stats shape (shape.estimate inputInfos)
  }
  where
    shape = taskShape t

-- | Input files, with their sizes.
taskInputs :: EstimatedTask a -> Set SizedTaskFile
taskInputs = (.inputs) . taskEstimation

-- | Output files, with their size estimates.
taskOutputs :: EstimatedTask a -> Set SizedTaskFile
taskOutputs = (.outputs) . taskEstimation

-- | See 'TaskEstimation'.
taskResourceEstimates :: EstimatedTask a -> ResourceEstimates
taskResourceEstimates = (.estimates) . taskEstimation

-- | Estimated memory in bytes: the figure the task is scheduled on, whether
-- that is the task's own prediction or one measured from statistics.
taskMemoryEstimate :: EstimatedTask a -> MemorySize
taskMemoryEstimate t = schedulingEstimate (taskResourceEstimates t).memory

-- | Estimated runtime in seconds, as a function of NumCPUs. See
-- 'taskMemoryEstimate'.
taskRuntimeEstimate :: EstimatedTask a -> NumCPUs -> NominalDiffTime
taskRuntimeEstimate t = schedulingEstimate (taskResourceEstimates t).runtime

-- Returns min(nodeMemory, taskMemory t)
taskMemoryCapped :: IsTask a => MemorySize -> EstimatedTask a -> MemorySize
taskMemoryCapped nodeMemory t =
  let
    memEstimate = taskMemoryEstimate t
  in
    if memEstimate > nodeMemory
    then Debug.trace (concat
      [ "WARNING: task memEstimate exceeds nodeMemory."
      , " Replacing it with nodeMemory. This might lead to a crash."
      , " memEstimate = ", show memEstimate
      , ", nodeMemory = ", show nodeMemory
      , ", task.tag = ", show (taskTag t)
      ]) nodeMemory
    else memEstimate

-- | Statistics prepared for the tasks of a map. A stat key's recorded input
-- summaries are decoded, and the model evaluated at them, once however many
-- tasks share the key; likewise for file stat keys.
data PreparedStats = MkPreparedStats
  { stats      :: TaskAndFileStats
  , closeTasks
      :: Map StatKey (EncodedSummary -> [(ResourceEstimates, TaskResourceMap)])
  , closeFiles
      :: Map FileStatKey (EncodedSummary -> [(FileSize, Trials FileSize)])
  }

-- | Prepare statistics for tasks of the given shapes. The model of a key is
-- taken from the first shape with that key: equal keys have equal models.
prepareStats :: TaskAndFileStats -> [TaskShape] -> PreparedStats
prepareStats stats shapes = MkPreparedStats
  { stats      = stats
  , closeTasks = firstByKey
      [ (key, closeObservations model (lookupTaskStats key stats))
      | shape <- shapes
      , Just key <- [shape.statKey]
      , Just model <- [shape.model]
      ]
  , closeFiles = firstByKey
      [ (key, closeObservations model (lookupFileStats key stats))
      | shape <- shapes
      , file <- Set.toList shape.outputFiles
      , Just key <- [file.fileStatKey]
      , Just model <- [Map.lookup file.path shape.outputModels]
      ]
  }
  where
    firstByKey :: Ord k => [(k, v)] -> Map k v
    firstByKey = Map.fromListWith (\_ first -> first)

-- | The observations recorded with summaries close to the given one, each with
-- the model's figure at its own summary. The recorded summaries are decoded and
-- evaluated once, when this is applied to them.
closeObservations
  :: Model r -> Map EncodedSummary obs -> EncodedSummary -> [(r, obs)]
closeObservations (MkModel decode estimateAt isClose) recorded = \own ->
  case decode own of
    Nothing -> []
    Just own' ->
      [ (r, o) | (s, typed, r, o) <- decoded, s /= own, isClose own' typed ]
  where
    decoded = [ (s, typed, estimateAt typed, o)
              | (s, o) <- Map.toList recorded
              , Just typed <- [decode s]
              ]

-- | Update memory, runtime and output file size estimates using statistics.
-- Input files keep their sizes: those come from the tasks producing them, after
-- statistics, or from the disk.
--
-- For each figure, best first:
--
-- 1. Statistics recorded for the same stat key and input summary: the
--    measured figure ('MeasuredFromStats').
-- 2. Statistics recorded for the same stat key and close input summaries
--    ('Hyperion.Scheduler.StatKey.closeInputSummaries'): the task's own model,
--    corrected by how far the measurements were from the model's predictions
--    for their inputs ('CorrectedByStats'). See 'memoryCorrection',
--    'runtimeCorrection' and 'fileSizeCorrection'.
-- 3. Otherwise the task's own model.
--
-- A miss is not reported here, but it is visible in the resulting
-- 'Estimate's, which 'runTasks' reports. Memory and runtime are replaced
-- independently: memory statistics are absent whenever no run recorded a
-- memory figure, while runtime statistics are always recorded.
applyStats :: PreparedStats -> TaskShape -> TaskEstimation -> TaskEstimation
applyStats prepared shape estimation = estimation
  { outputs   = Set.map applyFileStats estimation.outputs
  , estimates = MkResourceEstimates { memory = memory, runtime = runtime }
  }
  where
    own = estimation.estimates
    exact = do
      key <- shape.statKey
      summary <- estimation.inputSummary
      Map.lookup summary (lookupTaskStats key prepared.stats)
    close = fromMaybe [] $ do
      key <- shape.statKey
      summary <- estimation.inputSummary
      closeTo <- Map.lookup key prepared.closeTasks
      pure (closeTo summary)
    memory
      | Just measured <- exact >>= maxMemory =
          overrideWithMeasured measured own.memory
      | Just factor <- memoryCorrection (closeModels (.memory)) =
          scaled factor own.memory
      | otherwise = own.memory
    runtime
      | Just measured <- exact >>= approxRuntime Nothing =
          overrideWithMeasured measured own.runtime
      | Just (factor, corrected) <- runtimeCorrection ownCurve closeCurves =
          correctWithStats factor corrected own.runtime
      | otherwise = own.runtime
      where
        ownCurve = modelEstimate own.runtime
        closeCurves = closeModels (.runtime)
    -- Each close observation with the model's figure at its summary.
    closeModels :: (ResourceEstimates -> Estimate x) -> [(x, TaskResourceMap)]
    closeModels figure =
      [ (modelEstimate (figure model), resources)
      | (model, resources) <- close
      ]

    -- A file with no stat key is never looked up and keeps its estimate.
    applyFileStats info = info { fileSize = fileSize } where
      recordedSizes =
        maybe Map.empty (`lookupFileStats` prepared.stats) info.fileStatKey
      exactSize = estimation.producerSummary >>= (`Map.lookup` recordedSizes)
      closeSizes =
        fromMaybe [] (info.fileStatKey >>= (`Map.lookup` closeSizesByKey))
      fileSize
        | Just trials <- exactSize =
            overrideWithMeasured trials.max info.fileSize
        | Just factor <- fileSizeCorrection closeSizes =
            scaled factor info.fileSize
        | otherwise = info.fileSize

    -- By file stat key, so that the producer summary is decoded once per key
    -- rather than once per output file. Lazy: a key whose files all match
    -- exactly is never decoded.
    closeSizesByKey = case estimation.producerSummary of
      Nothing      -> Map.empty
      Just summary -> LazyMap.fromSet
        (\key -> maybe [] ($ summary) (Map.lookup key prepared.closeFiles))
        outputKeys
    outputKeys = Set.fromList
      [ key
      | info <- Set.toList estimation.outputs
      , Just key <- [info.fileStatKey]
      ]

    -- The task's own figure, scaled by the correction factor.
    scaled :: Integral a => Double -> Estimate a -> Estimate a
    scaled factor e = correctWithStats factor corrected e
      where corrected = ceiling (factor * fromIntegral (modelEstimate e))
