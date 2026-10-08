{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Task maps whose tasks carry their final estimates. The constructor is not
-- exported: 'mkEstimatedTaskMap' is the only way to make one, so a map passed
-- to 'Hyperion.Scheduler.RunTasks.runTasks' cannot have skipped it.
module Hyperion.Scheduler.Task.EstimatedTaskMap
  ( EstimatedTaskMap
  , estimatedTasks
  , mkEstimatedTaskMap
  ) where

import Control.Monad                         (unless)
import Control.Monad.Catch                   (MonadThrow, throwM)
import Data.Graph                            (flattenSCCs, stronglyConnComp)
import Data.Map.Strict                       qualified as Map
import Data.Maybe                            (catMaybes)
import Data.Set                              qualified as Set
import Hyperion.OsString                     (showOs)
import Hyperion.Scheduler.FilePath           (VirtualFilePath (..))
import Hyperion.Scheduler.StatKey            (SizedTaskFile (..), TaskFile (..),
                                              withSize)
import Hyperion.Scheduler.Stats              (TaskAndFileStats)
import Hyperion.Scheduler.Task.EstimatedTask (EstimatedTask, estimateTask,
                                              taskEstimation)
import Hyperion.Scheduler.Task.IsTask        (IsTask (..), TaskEstimation (..),
                                              TaskShape (..), taskOutputPaths)
import Hyperion.Scheduler.Task.TaskMap       (InvalidTaskMap (..), TaskMap,
                                              updateTaskMap, validateTaskMap)
import Hyperion.Scheduler.TaskFiles          (MonadTaskFiles (..))
import Hyperion.Scheduler.Types              (Estimate (..))

-- | A task map made by 'mkEstimatedTaskMap'.
newtype EstimatedTaskMap a = MkEstimatedTaskMap (TaskMap (EstimatedTask a))

estimatedTasks :: EstimatedTaskMap a -> TaskMap (EstimatedTask a)
estimatedTasks (MkEstimatedTaskMap taskMap) = taskMap

-- | Validate the map, compute every task's estimates from the sizes of its
-- input files, and apply statistics. Call it once, on the final map, e.g.
-- after replacing placeholders.
--
-- Tasks are estimated in dependency order, so an input file produced by a
-- task in the map gets that task's output file info, after statistics. Any
-- other input must already be on disk, and gets its size from there.
--
-- Throws 'InvalidTaskMap' if 'validateTaskMap' rejects the map, or if an input
-- is neither produced in the map nor on disk. Then every input of every task
-- has a size.
mkEstimatedTaskMap
  :: (IsTask a, MonadTaskFiles m, MonadThrow m)
  => TaskAndFileStats -> TaskMap a -> m (EstimatedTaskMap a)
mkEstimatedTaskMap stats taskMap = do
  validateTaskMap taskMap
  onDisk <- Map.fromList . catMaybes <$>
    traverse withDiskSize (Set.toList diskInputs)
  let missing = Set.filter (\i -> Map.notMember i.path onDisk) diskInputs
  unless (Set.null missing) $ throwM $ InvalidTaskMap $
    "Input files are produced by no task in the map and are not on disk: "
    <> showOs [ path | VirtualFilePath path <- (.path) <$> Set.toList missing ]
  let (estimated, _) = foldl' estimate (Map.empty, onDisk) dependenciesFirst
  pure $ MkEstimatedTaskMap $ updateTaskMap (estimated Map.!) taskMap
  where
    -- 'stronglyConnComp' lists a task after the tasks it depends on. The map
    -- is valid, so it has no cycles.
    dependenciesFirst = flattenSCCs $ stronglyConnComp
      [ (t, t, Set.toList deps) | (t, deps) <- Map.toList taskMap ]
    -- Estimate a task from the file infos known so far, and add its outputs.
    -- Its inputs are among them: the map is valid, so each produced input
    -- comes from a dependency, estimated earlier.
    estimate (!estimated, !known) t =
      (Map.insert t t' estimated, Map.union known outputs)
      where
        t' = estimateTask stats (knownIn known) t
        outputs = Map.fromList
          [ (o.path, o) | o <- Set.toList (taskEstimation t').outputs ]
    knownIn known file = case Map.lookup file.path known of
      Just info -> info
      Nothing   ->
        error $ "mkEstimatedTaskMap: no info for input " <> show file.path
    producedPaths = Set.unions $ map taskOutputPaths $ Map.keys taskMap
    diskInputs = Set.filter (\i -> Set.notMember i.path producedPaths) $
      Set.unions [ (taskShape t).inputFiles | t <- Map.keys taskMap ]
    withDiskSize i = do
      let VirtualFilePath path = i.path
      fmap (\size -> (i.path, withSize (EstimatedByTask size) i)) <$>
        taskFileSize path
