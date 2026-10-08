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
import Control.Monad                       (foldM, foldM_, unless)
import Control.Monad.Catch                 (MonadThrow, throwM)
import Data.Graph                          (SCC (..), stronglyConnComp)
import Data.List.NonEmpty                  qualified as NonEmpty
import Data.Map.Strict                     (Map)
import Data.Map.Strict                     qualified as Map
import Data.Set                            (Set)
import Data.Set                            qualified as Set
import Data.Typeable                       (Typeable)
import Hyperion.OsString                   (OsString, showOs)
import Hyperion.Scheduler.Config           (Config)
import Hyperion.Scheduler.FilePath         (VirtualFilePath (..), isNodeLocal)
import Hyperion.Scheduler.Task.IsTask      (IsTask (..), RunStage (..),
                                            taskHasClosure, taskInputPaths,
                                            taskOutputPaths)
import Hyperion.Scheduler.Task.TaskLink    (HasTaskChain (..), toTaskEdges)
import Hyperion.Scheduler.Task.WrappedTask (WrappedTask)

type TaskMap a = Map a (Set a)

-- | The tasks needed to create the key's files. Their estimates are not
-- computed yet: call
-- 'Hyperion.Scheduler.Task.EstimatedTaskMap.mkEstimatedTaskMap' on the final
-- map.
mkTaskMap :: (Monad m, HasTaskChain m r c k) => r -> c -> k -> m (TaskMap WrappedTask)
mkTaskMap resolver cfg key = toTaskEdges (taskChain resolver cfg) key

-- | Update all tasks (keys and values) in a TaskMap. A dependency in the
-- values becomes the updated task from the keys, so that each task is one
-- object: a 'WrappedTask' computes its fields lazily, once per object.
updateTaskMap :: Ord b => (a -> b) -> TaskMap a -> TaskMap b
updateTaskMap updateTask taskMap = Map.map (Set.map asKey) updatedKeys
  where
    updatedKeys = Map.mapKeys updateTask taskMap
    asKey dep = case Map.lookupIndex dep' updatedKeys of
      Just i  -> fst (Map.elemAt i updatedKeys)
      Nothing -> dep'
      where
        dep' = updateTask dep

-- | Check that every node-local input is produced by a task in the map. The
-- file service knows nothing of node-local files from before the run, so a
-- consumer could never get one: the map was built with node-local paths
-- visible, e.g. outside 'Hyperion.Scheduler.TaskFiles.runTaskFiles'.
validateNodeLocalInputs
  :: (IsTask a, MonadThrow m) => Config -> TaskMap a -> m ()
validateNodeLocalInputs config taskMap =
  unless (Set.null orphans) $ throwM $ InvalidTaskMap $
    "Node-local input files are produced by no task in the map. Was the map \
    \built outside runTaskFiles? "
    <> showOs [ path | VirtualFilePath path <- Set.toList orphans ]
  where
    produced = Set.unions $ map taskOutputPaths $ Map.keys taskMap
    orphans = Set.filter orphan $
      Set.unions $ map taskInputPaths $ Map.keys taskMap
    orphan p = isNodeLocal config p && Set.notMember p produced

newtype InvalidTaskMap = InvalidTaskMap OsString
  deriving (Show)

instance Exception InvalidTaskMap

-- | Check that task map is valid:
-- - All tasks are in map keys
-- - No unreplaced placeholder tasks (see 'Hyperion.Scheduler.Task.Task.TaskKind')
-- - No circular dependencies
-- - No duplicate output paths.
-- - Each input path produced by some task in the map can be found in output
--   paths of dependencies (input paths produced by no task in the map are
--   assumed to already exist on disk -- see 'assertCorrectDependencyPaths')
-- - Every task with work to run remotely asks for at least one CPU
--   (see 'assertComputeTasksHaveCpus')
-- - No task declares maxThreads < minThreads
--   (see 'assertMinMaxThreads')
validateTaskMap :: (IsTask a, MonadThrow m) => TaskMap a -> m ()
validateTaskMap taskMap = do
  assertAllTasksAreInKeys
  assertNoPlaceholders
  assertNoCycles
  assertNoDuplicatePaths
  assertCorrectDependencyPaths
  assertComputeTasksHaveCpus
  assertMinMaxThreads
  where
    assert cond msg = case cond of
      True  -> pure ()
      False -> throwM $ InvalidTaskMap msg

    taskLabel t = (taskTag t, taskOutputPaths t)

    stages = [InitialRun, InProgressRun]

    assertAllTasksAreInKeys = do
      let
        keys = Map.keysSet taskMap
        depKeys = Set.unions $ Map.elems taskMap
        missingKeys = Set.toList $ Set.difference depKeys keys
      assert (null missingKeys) $
        "Tasks are missing from TaskMap keys: " <>
        showOs (map taskLabel missingKeys)

    assertNoPlaceholders = do
      let placeholders = filter taskIsPlaceholder $ Map.keys taskMap
      assert (null placeholders) $
        "TaskMap contains unreplaced placeholder tasks: " <>
        showOs (map taskLabel placeholders)

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

    -- Any task that has to be run remotely needs to require at least 1 CPU.
    -- Otherwise, scheduler can (and probably will) assign 0 CPUs and throw an error, see
    -- 'Hyperion.Scheduler.RunTasks.RemoteRunTask.remoteRunTask'
    assertComputeTasksHaveCpus = assert (null starvable) $
      "Tasks have work to run remotely but declare minThreads <= 0, so the \
      \allocator may give them no CPUs and they would then fail for want of a \
      \worker: " <> showOs (map taskLabel starvable)
      where
        starvable =
          [ t
          | t <- Map.keys taskMap
          , taskHasClosure t
          , any (\stage -> taskMinThreads stage t <= 0) stages
          ]

    -- maxThreads >= minThreads
    assertMinMaxThreads = assert (null inverted) $
      "Tasks declare a maxThreads below their minThreads, which no allocation \
      \can satisfy (task, stage, minThreads, maxThreads): " <> showOs inverted
      where
        inverted =
          [ (taskLabel t, stage, taskMinThreads stage t, taskMaxThreads stage t)
          | t <- Map.keys taskMap
          , stage <- stages
          , taskMaxThreads stage t < taskMinThreads stage t
          ]

    -- Each input path for a task either:
    -- 1. Is produced by some task in the TaskMap
    -- or
    -- 2. Already exists on disk
    -- or should be in the task's dependencies outputs.
    -- In the first case, the input path should be in dependencies outputs - we check this.
    -- In the second case, the input path is not in any task's outputs, so we
    -- ignore it here: 'mkEstimatedTaskMap' checks that it exists.
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

-- | Replace each placeholder task with a subtree (actual task + its dependencies), as specified by replacementMap.
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

-- | Collect all placeholder tasks (see 'Hyperion.Scheduler.Task.Task.TaskKind')
-- with underlying key type @k@ appearing in a TaskMap, paired with the task
-- they came from - ready to be used as 'replaceTasks' keys.
placeholdersOfType :: (IsTask a, Typeable k) => TaskMap a -> [(a, k)]
placeholdersOfType taskMap =
  [ (t, k) | t <- Map.keys taskMap, Just k <- [taskPlaceholderKey t] ]
