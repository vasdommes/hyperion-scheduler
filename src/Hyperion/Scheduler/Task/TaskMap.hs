{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
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
import Data.Maybe                          (isJust, isNothing)
import Data.Set                            (Set)
import Data.Set                            qualified as Set
import Data.Text                           (Text)
import Data.Typeable                       (Typeable)
import Hyperion.OsString                   (OsString, showOs)
import Hyperion.Scheduler.StatKey          (TaskKeyFileInfo (..))
import Hyperion.Scheduler.Stats            (TaskAndFileStats)
import Hyperion.Scheduler.Task.IsTask      (IsTask (..), ResourceEstimates (..),
                                            RunStage (..), Tag, taskInputPaths,
                                            taskOutputPaths)
import Hyperion.Scheduler.Task.TaskLink    (HasTaskChain (..), toTaskEdges)
import Hyperion.Scheduler.Task.WrappedTask (WrappedTask, decorateTaskWithStats)
import Hyperion.Scheduler.Types            (isMeasuredFromStats, modelEstimate,
                                            schedulingEstimate)

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
-- - No unreplaced placeholder tasks (see 'Hyperion.Scheduler.Task.Task.TaskKind')
-- - No circular dependencies
-- - No duplicate output paths.
-- - Each input path produced by some task in the map can be found in output
--   paths of dependencies (input paths produced by no task in the map are
--   assumed to already exist on disk -- see 'assertCorrectDependencyPaths')
-- - Every task with work to run remotely asks for at least one CPU
--   (see 'assertComputeTasksHaveCpus')
-- - No task declares a maxThreads below its minThreads
--   (see 'assertThreadRangesAreOrdered')
validateTaskMap :: (IsTask a, MonadThrow m) => TaskMap a -> m ()
validateTaskMap taskMap = do
  assertAllTasksAreInKeys
  assertNoPlaceholders
  assertNoCycles
  assertNoDuplicatePaths
  assertCorrectDependencyPaths
  assertComputeTasksHaveCpus
  assertThreadRangesAreOrdered
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

    -- A task that has something to run remotely needs a worker to run it on,
    -- and a worker is what a CPU buys, so a minimum of zero is a declaration
    -- that contradicts itself: 'Hyperion.Scheduler.RunTasks.RemoteRunTask.remoteRunTask'
    -- throws when it holds a closure and no worker.
    --
    -- Rejected rather than warned about, because 'taskMinThreads' is a floor,
    -- not the allocation. Such a task may be given CPUs for hours and then be
    -- drained to none -- 'refineAllocation' takes threads from the fastest task
    -- down to exactly this floor -- so the failure is load-dependent and
    -- arrives once dependencies have already been computed.
    assertComputeTasksHaveCpus = assert (null starvable) $
      "Tasks have work to run remotely but declare minThreads = 0, so the \
      \allocator may give them no CPUs and they would then fail for want of a \
      \worker: " <> showOs (map taskLabel starvable)
      where
        starvable =
          [ t
          | t <- Map.keys taskMap
          , isJust (taskClosure 0 t)
          , any (\stage -> taskMinThreads stage t == 0) stages
          ]

    -- An allocation may give a task no fewer threads than its minimum and no
    -- more than its maximum, so a maximum below the minimum can be satisfied by
    -- nothing. 'basicValidAllocation' resolves the contradiction by capping at
    -- the maximum, handing out fewer threads than the task asked for -- none at
    -- all when the maximum is zero, which fails the same way
    -- 'assertComputeTasksHaveCpus' describes, and reaches a task whose minimum
    -- is positive.
    assertThreadRangesAreOrdered = assert (null inverted) $
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

-- | A way in which a task is not instrumented, i.e. will be scheduled on a
-- guess rather than on a declared or measured figure.
--
-- These read what a task /declares/, never a figure recovered from statistics:
-- a task that declares nothing is a gap whether or not this particular run
-- happens to have history for it, since the next machine may have none.
--
-- There are two independent axes -- task statistics and file statistics -- and
-- the two gaps on each axis are mutually exclusive. A task is therefore
-- reported at most once per axis: one that declares no stat key at all is not
-- also reported for estimating zero memory, since that follows from the first
-- and has a different remedy.
data InstrumentationGap
  = NoStatKey
  | ZeroMemoryEstimate
  | NoFileStatKey
  | ZeroFileSizeEstimate
  deriving (Eq, Ord, Show, Enum, Bounded)

describeInstrumentationGap :: InstrumentationGap -> Text
describeInstrumentationGap = \case
  NoStatKey ->
    "declare no stat key: neither estimated nor recorded"
  ZeroMemoryEstimate ->
    "have a stat key, but its memoryEstimate is zero"
  NoFileStatKey ->
    "produce output files, but none of them declares a file stat key"
  ZeroFileSizeEstimate ->
    "have file stat keys, but every output file's size is zero"

-- | Instrumentation gaps in a task map, with the tags of the tasks affected.
-- Deduplicated, so there is one entry per task type rather than per task.
--
-- These are diagnostics, not errors: they are reported by 'runTasks' rather
-- than rejected by 'validateTaskMap', because a zero estimate is harmless for
-- a small task and only costs an under-allocation for a large one. Tasks that
-- compute nothing (no-ops, placeholders) are excluded entirely -- for them,
-- declaring nothing is correct.
taskInstrumentationGaps :: IsTask a => TaskMap a -> Map InstrumentationGap (Set (Maybe Tag))
taskInstrumentationGaps taskMap = Map.fromListWith Set.union
  [ (gap, Set.singleton (taskTag t))
  | t <- Map.keys taskMap
  , isJust (taskClosure 1 t)
  , gap <- statGaps t <> fileGaps t
  ]
  where
    statGaps t
      | isNothing (taskStatKey t) = [NoStatKey]
      | declaredMemory t == 0      = [ZeroMemoryEstimate]
      | otherwise                 = []

    declaredMemory t = modelEstimate (taskResourceEstimates t).memory

    fileGaps t
      | Set.null outputs                              = []
      | all (isNothing . (.fileStatKey)) outputsList  = [NoFileStatKey]
      | all ((== 0) . modelEstimate . (.fileSize)) outputsList = [ZeroFileSizeEstimate]
      | otherwise                                     = []
      where
        outputs = taskOutputs t
        outputsList = Set.toList outputs

-- | How many tasks are running on recorded statistics rather than on their own
-- models, out of those that could be: a task with no stat key can never match,
-- so counting it would dilute the ratio and make coverage always look poor.
--
-- A ratio near zero after statistics were supplied usually means the keys did
-- not match at all -- a renamed stat key type, or a key whose shape changed --
-- rather than a genuine absence of history.
statsCoverage :: IsTask a => TaskMap a -> (Int, Int)
statsCoverage taskMap = (length measured, length couldMatch)
  where
    couldMatch = filter (isJust . taskStatKey) (Map.keys taskMap)
    measured = filter isMeasured couldMatch
    isMeasured t = isMeasuredFromStats estimates.memory
      || isMeasuredFromStats estimates.runtime
      where estimates = taskResourceEstimates t

-- | Tags of tasks whose recorded memory exceeded their own estimate by more
-- than the given factor, i.e. whose model is optimistic. These are the tasks
-- most likely to be killed for running out of memory on a machine with no
-- statistics to fall back on.
--
-- Only tasks whose estimates actually came from statistics can be compared, so
-- a task that has never run is never reported.
underestimatedMemoryTags :: IsTask a => Rational -> TaskMap a -> Set (Maybe Tag)
underestimatedMemoryTags factor taskMap = Set.fromList
  [ taskTag t
  | t <- Map.keys taskMap
  , let memory = (taskResourceEstimates t).memory
  , isMeasuredFromStats memory
  , toRational (schedulingEstimate memory) > factor * toRational (modelEstimate memory)
  ]
