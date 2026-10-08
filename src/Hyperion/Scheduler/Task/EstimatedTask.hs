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
import Data.Set                       (Set)
import Data.Set                       qualified as Set
import Data.Time.Clock                (NominalDiffTime)
import Debug.Trace                    qualified as Debug
import Hyperion.Scheduler.StatKey     (SizedTaskFile (..))
import Hyperion.Scheduler.Stats       (TaskAndFileStats, approxRuntime,
                                       lookupMaxFileSize, lookupTaskStats,
                                       maxMemory)
import Hyperion.Scheduler.Task.IsTask (InputInfos, IsTask (..),
                                       ResourceEstimates (..),
                                       TaskEstimation (..), TaskShape (..))
import Hyperion.Scheduler.Types       (MemorySize, NumCPUs,
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
estimateTask
  :: IsTask a => TaskAndFileStats -> InputInfos -> a -> EstimatedTask a
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

-- | Update memory, runtime and output file size estimates using statistics.
-- Input files keep their sizes: those come from the tasks producing them, after
-- statistics, or from the disk.
--
-- Lookup is an exact match on the stat key, so a task whose key has changed
-- (a new estimate-relevant config value, say) misses and keeps its own
-- estimate; a task with no stat key is never looked up at all. A miss shows
-- in the resulting 'Estimate's.
--
-- Memory and runtime are replaced independently: memory statistics are absent
-- whenever no run recorded a memory figure, while runtime statistics are
-- always recorded, so a task can end up running on a measured runtime and its
-- own memory estimate.
applyStats :: TaskAndFileStats -> TaskShape -> TaskEstimation -> TaskEstimation
applyStats stats shape estimation = estimation
  { outputs   = Set.map applyFileStats estimation.outputs
  , estimates = MkResourceEstimates
      { memory  = maybe id overrideWithMeasured measuredMemory own.memory
      , runtime = maybe id overrideWithMeasured measuredRuntime own.runtime
      }
  }
  where
    own = estimation.estimates
    recorded = flip lookupTaskStats stats =<< shape.statKey
    measuredRuntime = recorded >>= approxRuntime Nothing
    measuredMemory  = recorded >>= maxMemory

    -- A file with no stat key is never looked up and keeps its estimate.
    applyFileStats info = info { fileSize = fileSize } where
      fileSize = maybe id overrideWithMeasured measured info.fileSize
      measured = flip lookupMaxFileSize stats =<< info.fileStatKey
