{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}

-- | Task maps whose tasks carry their final estimates. The constructors are
-- not exported: 'mkEstimatedTaskMap' is the only way to make one, so a map
-- passed to 'Hyperion.Scheduler.RunTasks.runTasks' cannot have skipped it.
module Hyperion.Scheduler.Task.EstimatedTaskMap
  ( EstimatedTask
  , originalTask
  , EstimatedTaskMap
  , estimatedTasks
  , mkEstimatedTaskMap
  ) where

import Data.Aeson                          (ToJSON (..))
import Hyperion.Scheduler.Stats            (TaskAndFileStats)
import Hyperion.Scheduler.Task.IsTask      (IsTask (..), TaskSummary (..))
import Hyperion.Scheduler.Task.TaskMap     (TaskMap, updateTaskMap)
import Hyperion.Scheduler.Task.WrappedTask (decorateSummaryWithStats)

-- | A task with its final summary, after statistics.
data EstimatedTask a = MkEstimatedTask
  { task    :: a
  , summary :: TaskSummary
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
  taskSummary t          = t.summary
  taskMaxThreads stage t = taskMaxThreads stage t.task
  taskMinThreads stage t = taskMinThreads stage t.task
  taskDefaultPriority t  = taskDefaultPriority t.task
  taskTag t              = taskTag t.task
  taskClosure t          = taskClosure t.task
  taskIsPlaceholder t    = taskIsPlaceholder t.task
  taskPlaceholderKey t   = taskPlaceholderKey t.task

-- | A task map made by 'mkEstimatedTaskMap'.
newtype EstimatedTaskMap a = MkEstimatedTaskMap (TaskMap (EstimatedTask a))
  deriving newtype (Semigroup, Monoid)

estimatedTasks :: EstimatedTaskMap a -> TaskMap (EstimatedTask a)
estimatedTasks (MkEstimatedTaskMap taskMap) = taskMap

-- | Apply statistics to every task's estimates. Call it once, on the final
-- map, e.g. after replacing placeholders.
mkEstimatedTaskMap :: IsTask a => TaskAndFileStats -> TaskMap a -> EstimatedTaskMap a
mkEstimatedTaskMap stats = MkEstimatedTaskMap . updateTaskMap estimate
  where
    estimate t = MkEstimatedTask
      { task    = t
      , summary = decorateSummaryWithStats stats (taskSummary t)
      }
