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
import Data.Graph                          (flattenSCCs, stronglyConnComp)
import Data.Map.Strict                     qualified as Map
import Data.Maybe                          (fromMaybe)
import Data.Set                            qualified as Set
import Hyperion.Scheduler.FilePath         (VirtualFilePath (..))
import Hyperion.Scheduler.StatKey          (TaskKeyFileInfo (..))
import Hyperion.Scheduler.Stats            (TaskAndFileStats)
import Hyperion.Scheduler.Task.IsTask      (IsTask (..), TaskSummary (..),
                                            taskInputs, taskOutputPaths)
import Hyperion.Scheduler.Task.TaskMap     (TaskMap, updateTaskMap)
import Hyperion.Scheduler.Task.WrappedTask (decorateSummaryWithStats)
import Hyperion.Scheduler.TaskFiles        (MonadTaskFiles (..))
import Hyperion.Scheduler.Types            (Estimate (..))

-- | A task with its final summary: estimates from the sizes of its input
-- files, after statistics.
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
  taskSummary Nothing t     = t.summary
  taskSummary knownInputs t = taskSummary knownInputs t.task
  taskMaxThreads stage t    = taskMaxThreads stage t.task
  taskMinThreads stage t    = taskMinThreads stage t.task
  taskDefaultPriority t     = taskDefaultPriority t.task
  taskTag t                 = taskTag t.task
  taskClosure t             = taskClosure t.task
  taskIsPlaceholder t       = taskIsPlaceholder t.task
  taskPlaceholderKey t      = taskPlaceholderKey t.task

-- | A task map made by 'mkEstimatedTaskMap'.
newtype EstimatedTaskMap a = MkEstimatedTaskMap (TaskMap (EstimatedTask a))
  deriving newtype (Semigroup, Monoid)

estimatedTasks :: EstimatedTaskMap a -> TaskMap (EstimatedTask a)
estimatedTasks (MkEstimatedTaskMap taskMap) = taskMap

-- | Compute every task's estimates from the sizes of its input files, and
-- apply statistics. Call it once, on the final map, e.g. after replacing
-- placeholders.
--
-- Tasks are estimated in dependency order, so an input file produced by a
-- task in the map gets that task's output file info, after statistics. Any
-- other input is already on disk, and gets its size from there. In a cycle,
-- which 'Hyperion.Scheduler.Task.TaskMap.validateTaskMap' rejects, some
-- produced inputs keep their own infos.
mkEstimatedTaskMap
  :: (IsTask a, MonadTaskFiles m)
  => TaskAndFileStats -> TaskMap a -> m (EstimatedTaskMap a)
mkEstimatedTaskMap stats taskMap = do
  onDisk <- Map.fromList <$> traverse withDiskSize (Set.toList diskInputs)
  let (estimated, _) = foldl' estimate (Map.empty, onDisk) dependenciesFirst
  pure $ MkEstimatedTaskMap $ updateTaskMap (lookupEstimated estimated) taskMap
  where
    -- 'stronglyConnComp' lists a task after the tasks it depends on.
    dependenciesFirst = flattenSCCs $ stronglyConnComp
      [ (t, t, Set.toList deps) | (t, deps) <- Map.toList taskMap ]
    -- Estimate a task from the file infos known so far, and add its outputs.
    estimate (!estimated, !known) t = (Map.insert t t' estimated, Map.union known outputs)
      where
        t' = MkEstimatedTask
          { task    = t
          , summary = decorateSummaryWithStats stats $ taskSummary (Just (`Map.lookup` known)) t
          }
        outputs = Map.fromList [ (o.path, o) | o <- Set.toList t'.summary.outputs ]
    -- A dependency missing from the keys, which 'validateTaskMap' rejects,
    -- keeps its own summary.
    lookupEstimated estimated t = Map.findWithDefault
      (MkEstimatedTask { task = t, summary = taskSummary Nothing t }) t estimated
    producedPaths = Set.unions $ map taskOutputPaths $ Map.keys taskMap
    diskInputs = Set.filter (\i -> Set.notMember i.path producedPaths) $
      Set.unions $ map taskInputs $ Map.keys taskMap
    -- A missing file keeps the unknown size 0.
    withDiskSize i = do
      let VirtualFilePath path = i.path
      size <- taskFileSize path
      pure (i.path, i { fileSize = EstimatedByTask (fromMaybe 0 size) })
