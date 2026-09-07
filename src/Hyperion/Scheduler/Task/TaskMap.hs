{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StaticPointers        #-}
{-# LANGUAGE UndecidableInstances  #-}

module Hyperion.Scheduler.Task.TaskMap where

import Control.Exception                   (Exception)
import Control.Monad                       (foldM, foldM_)
import Control.Monad.Catch                 (MonadThrow, throwM)
import Data.Graph                          (SCC (..), stronglyConnComp)
import Data.List.NonEmpty                  qualified as NonEmpty
import Data.Map.Strict                     (Map)
import Data.Map.Strict                     qualified as Map
import Data.Set                            (Set)
import Data.Set                            qualified as Set
import Hyperion.OsString                   (OsString, showOs)
import Hyperion.Scheduler.Stats            (TaskAndFileStats)
import Hyperion.Scheduler.Task.IsTask      (IsTask (..), taskInputPaths,
                                            taskOutputPaths)
import Hyperion.Scheduler.Task.TaskLink    (HasTaskChain (..), toTaskEdges)
import Hyperion.Scheduler.Task.WrappedTask (WrappedTask, decorateTaskWithStats)

type TaskMap a = Map a (Set a)

mkTaskMap :: (Monad m, HasTaskChain m r c k) => r -> c -> k -> m (TaskMap WrappedTask)
mkTaskMap resolver cfg = toTaskEdges (taskChain resolver cfg)

-- Update all tasks (keys and values) in a TaskMap
updateTaskMap :: Ord b => (a -> b) -> TaskMap a -> TaskMap b
updateTaskMap updateTask = updateKeys . updateValues where
  updateKeys = Map.mapKeys updateTask
  updateValues = Map.map $ Set.map updateTask

decorateTaskMapWithStats :: TaskAndFileStats -> TaskMap WrappedTask -> TaskMap WrappedTask
decorateTaskMapWithStats = updateTaskMap . decorateTaskWithStats

newtype InvalidTaskMap = InvalidTaskMap OsString
  deriving (Show)

instance Exception InvalidTaskMap

-- | Check that task map is valid:
-- - All tasks are in map keys
-- - No circular dependencies
-- - No duplicate output paths.
-- - Each input path produced by some task in the map can be found in output
--   paths of dependencies (input paths produced by no task in the map are
--   assumed to already exist on disk -- see 'assertCorrectDependencyPaths')
validateTaskMap :: (IsTask a, MonadThrow m) => TaskMap a -> m ()
validateTaskMap taskMap = do
  assertAllTasksAreInKeys
  assertNoCycles
  assertNoDuplicatePaths
  assertCorrectDependencyPaths
  where
    assert cond msg = case cond of
      True  -> pure ()
      False -> throwM $ InvalidTaskMap msg

    taskLabel t = (taskTag t, taskOutputPaths t)

    assertAllTasksAreInKeys = do
      let
        keys = Map.keysSet taskMap
        depKeys = Set.unions $ Map.elems taskMap
        missingKeys = Set.toList $ Set.difference depKeys keys
      assert (null missingKeys) $
        "Tasks are missing from TaskMap keys: " <>
        showOs (map taskLabel missingKeys)

    assertNoCycles = assert (null cycles) $ "TaskMap contains cycles: " <> showOs (map showCycle cycles) where
      edges = [(k, k, Set.toList ks) | (k, ks) <- Map.toList taskMap]
      cycles = [c | NECyclicSCC c <- stronglyConnComp edges]
      showCycle = map taskLabel . NonEmpty.toList

    assertNoDuplicatePaths = foldM_ go Set.empty (Map.keys taskMap) where
      go existingPaths task = foldM go' existingPaths (taskOutputPaths task)
      go' existingPaths path = do
        assert (Set.notMember path existingPaths) $
          "Duplicate path: " <> showOs path
        pure $ Set.insert path existingPaths

    -- Each input path for a task either:
    -- 1. Is produced by some task in the TaskMap
    -- or
    -- 2. Already exists on disk
    -- or should be in the task's dependencies outputs.
    -- In the first case, the input path should be in dependencies outputs - we check this.
    -- In the second case, the input path is not in any task's outputs, so we ignore it.
    -- TODO: call validateTaskMap in (MonadPathExists m) and check that path exists in the second case?
    assertCorrectDependencyPaths = mapM_ go (Map.toList taskMap) where
      allOutputPaths = Set.unions $ map taskOutputPaths $ Map.keys taskMap
      go (t, deps) = do
        let
          depsOutputs = Set.unions $ Set.map taskOutputPaths deps
          missingInputs =
            Set.toList $
            Set.intersection allOutputPaths $
            Set.difference (taskInputPaths t) depsOutputs
        assert (null missingInputs) $ "Task input paths are produced in this TaskMap, but not by the task's dependencies!" <>
          " Missing paths: " <> showOs missingInputs <>
          " task: " <> showOs (taskLabel t) <>
          " dependencies: " <> showOs (map taskLabel $ Set.toList deps)

-- | Replace each dummy task with a subtree (actual task + its dependencies), as specified by replacementMap.
replaceTasks :: IsTask a => Map a (TaskMap a) -> TaskMap a -> TaskMap a
replaceTasks replacementMap = addNewKeys . replaceDeps . removeOldKeys where
  -- removeOldKeys :: TaskMap a -> TaskMap a
  removeOldKeys taskMap = Map.withoutKeys taskMap tasksToReplace

  -- replaceDeps :: TaskMap a -> TaskMap a
  replaceDeps = Map.map updateDepsSet

  -- addNewKeys :: TaskMap a -> TaskMap a
  addNewKeys oldMap = Map.unionsWith (<>) (oldMap : Map.elems replacementMap)

  -- tasksToReplace :: Set a
  tasksToReplace = Map.keysSet replacementMap

  -- updateDepsSet :: Set a -> Set a
  updateDepsSet deps = Set.union toAdd $ Set.difference deps toRemove where
    toRemove = Set.intersection deps tasksToReplace
    toAdd = Set.unions $ Map.elems $ Map.restrictKeys taskReplacements toRemove

  -- Dependents of a replaced task are connected only to the roots of its
  -- replacement TaskMap. The rest is reachable through the roots.
  -- taskReplacements :: Map a (Set a)
  taskReplacements = Map.map roots replacementMap
  roots taskMap = Set.difference keys depKeys where
    keys = Map.keysSet taskMap
    depKeys = Set.unions $ Map.elems taskMap
