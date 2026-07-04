{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Hyperion.Scheduler.RunTasks.TaskDistribution where

import Data.List                          qualified as List
import Data.Map.Strict                    (Map)
import Data.Map.Strict                    qualified as Map
import Data.Maybe                         (mapMaybe)
import Data.MinMaxQueue                   qualified as MinMaxQueue
import Data.Ord                           (Down (..))
import Data.Set                           (Set)
import Data.Set                           qualified as Set
import Data.Time.Clock                    (NominalDiffTime)
import Hyperion.Scheduler.Config          (Config)
import Hyperion.Scheduler.FilePath        (VirtualFilePath, isNodeLocal)
import Hyperion.Scheduler.IsTask (IsTask (..), RunStage (..),
        taskMemoryCapped)
import Hyperion.Scheduler.TaskKeyFileInfo (TaskKeyFileInfo (..))
import Hyperion.Scheduler.Types           (FileSize, Node (..), NumCPUs)

type CPUAllocation a = Map a NumCPUs

allocCompletionTime :: IsTask a => CPUAllocation a -> NominalDiffTime
allocCompletionTime alloc
  | Map.null alloc = 0
  | otherwise = maximum $
    [taskRuntimeEstimate task threads | (task, threads) <- Map.toList alloc]

basicValidAllocation :: IsTask a => RunStage -> NumCPUs -> [a] -> [(a, NumCPUs)]
basicValidAllocation stage totalCpus tasks = allocNoExcess
  where
    -- Minimum number of threads for each task
    allocMin = [(task, taskMinThreads stage task) | task <- tasks]

    -- Distribute 'e' excess CPUs over the given tasks, preferentially
    -- to the first tasks in the list, while not exceeding
    -- maxThreads. It doesn't really matter how we do this, since
    -- later we will call refineAllocation.
    distributeExcess 0 accum ts = accum ++ ts
    distributeExcess _ accum [] = accum
    distributeExcess e accum ((task, cpus) : ts) =
      let
        cpus' = min (cpus + e) (taskMaxThreads stage task)
        e'    = e - (cpus' - cpus)
      in
        distributeExcess e' ((task, cpus'):accum) ts

    excessCpus = totalCpus - sum (map snd allocMin)
    -- Use up all the excess CPUs by distributing them among the
    -- tasks. This allocation is sub-optimal.
    allocNoExcess = distributeExcess excessCpus [] allocMin

-- | Distribute CPUs among the given tasks. We try to distribute CPUs
-- in such a way that all tasks finish at the same time, while never
-- giving a task less than minThreads or more than maxThreads.
--
-- NB: This function is only well-defined when 'totalCpus' is between
-- total sum of 'minThreads' and the total sum of 'maxThreads'.
allocateCpusToTasks :: forall a . IsTask a => RunStage -> NumCPUs -> [a] -> CPUAllocation a
allocateCpusToTasks stage totalCpus tasks =
  let (fastTasks, normalTasks, slowTasks) = refineAllocation stage (basicValidAllocation stage totalCpus tasks)
  in Map.fromList $ fastTasks <> normalTasks <> slowTasks

-- Total size for all node-local input and output files of a task.
taskLocalFileSizeMap :: IsTask a => Config -> a -> Map VirtualFilePath FileSize
taskLocalFileSizeMap config t =
  Map.fromList $
  map (\fileInfo -> (fileInfo.path, fileInfo.fileSize)) $
  Set.toList $
  Set.filter (isNodeLocal config . (.path)) $
  Set.union (taskInputs t) (taskOutputs t)

canHandleTask :: IsTask a => Config -> a -> (Node, CPUAllocation a) -> Bool
canHandleTask config task (node, nodeAlloc) =
  total (taskMemoryCapped node.memory) <= node.memory &&
  total (taskMinThreads InitialRun)    <= node.cpus &&
  totalLocalFileSize                   <= node.localStorageSize
  where
    total f = sum $ map f thisAndNodeTasks
    thisAndNodeTasks = (task : Map.keys nodeAlloc)
    -- NB: in theory, different tasks can share some input files.
    -- Thus we have to merge file size maps instead of simply computing a sum over all tasks.
    -- In practice it does not matter, since we call distributeTasksToNodesWithScores
    -- only for initial allocation of independent tasks that don't have local input files at all.
    -- Note also that one should adjust node.localStorageSize before passing it to canHandleTask,
    -- if it is called in the middle of computation and local storage is already filled with other files.
    totalLocalFileSize =
      sum $ Map.elems $
      Map.unionsWith (+) $
      map (taskLocalFileSizeMap config) thisAndNodeTasks

getRuntime :: IsTask a => (a, NumCPUs) -> NominalDiffTime
getRuntime (t, threads) = taskRuntimeEstimate t threads

-- Now we iteratively refine the allocation by taking 1 CPU from
-- the fastest task and giving it to the slowest, until we no
-- longer speed things up. To do so, we maintain a MinMaxQueue of
-- tasks, with priority given by their runtime.
--
-- Returns a triple (tooFastTasks, normalTasks, slowTasks), where
--
-- tooFastTasks : tasks that are assigned the minimum number of
--                   threads, but are still faster than the
--                   others. This is a potential source of
--                   inefficiency. We should consider giving their
--                   cpus to other tasks and deferring them until
--                   later.
--
-- normalTasks : tasks that have cpus distributed to them in a
--                   reasonably optimal way
--
-- slowTasks : tasks that cannot be made faster by giving them more
--                   CPUs. These tasks should always be started
--                   immediatey, since they will likely be a
--                   bottleneck.
refineAllocation
  :: IsTask a
  => RunStage
  -> [(a, NumCPUs)]
  -> ([(a, NumCPUs)], [(a, NumCPUs)], [(a, NumCPUs)])
refineAllocation stage allocInit = go queueInit [] [] []
  where
    queueInit = MinMaxQueue.fromListWith getRuntime allocInit
    go queue tooFastTasks allocDone slowTasks =
      let
        allAssignments = (tooFastTasks, MinMaxQueue.elems queue ++ allocDone, slowTasks)
      in
        case MinMaxQueue.pollMin queue of
          Nothing -> allAssignments
          Just (fast@(fastestTask, fastCpus), queue') ->
            -- If the fastest task has minThreads, then it will never
            -- become the slowest task. We can add it to allocDone and move
            -- on.
            if fastCpus == taskMinThreads stage fastestTask
            then go queue' (fast : tooFastTasks) allocDone slowTasks
            else
              case MinMaxQueue.pollMax queue' of
                Nothing -> allAssignments
                Just (slow@(slowestTask, slowCpus), queue'') ->
                  -- If the slowest task already has its maximum number
                  -- of threads, there is nothing we can do to make it
                  -- faster. We can add it to slowTasks and move on. We
                  -- still try to make the other tasks faster, but this
                  -- one will always be a straggler.
                  if slowCpus == taskMaxThreads stage slowestTask
                  then go (MinMaxQueue.insert getRuntime fast queue'')
                       tooFastTasks allocDone (slow : slowTasks)
                  else
                    let
                      -- Try to speed things up by removing a thread
                      -- from the fastest task and giving it to the
                      -- slowest task
                      newFast = (fastestTask, fastCpus - 1)
                      newSlow = (slowestTask, slowCpus + 1)
                      newQueue =
                        MinMaxQueue.insert getRuntime newFast $
                        MinMaxQueue.insert getRuntime newSlow $
                        queue''
                    in
                      if max (getRuntime newFast) (getRuntime newSlow) <
                         max (getRuntime fast) (getRuntime slow)
                      then
                        -- We managed to make things faster, locally
                        go newQueue tooFastTasks allocDone slowTasks
                      else
                        -- If we didn't manage to make things faster
                        -- by stealing CPUs from the fastest task,
                        -- then it probably has the right number of
                        -- CPUs. Remove it from consideration and
                        -- continue refining the slower tasks. TODO:
                        -- should these tasks go into "tooFastTasks"?
                        go queue' tooFastTasks (fast : allocDone) slowTasks

-- | For each task, choose the node that can handle the task with the
-- lowest completion time and assign that task to that
-- node. Reallocate cpus on the node to optimize the completion
-- time. Return the node assignments and a list of any un-assigned
-- tasks.
--
-- TODO: Currently, we choose the node that will finish first,
-- before we add the next task. However, perhaps we should consider
-- adding the task to each node and see how long it will take to
-- finish. If we don't take that strategy, then we probably shouldn't
-- re-compute the completion time every round.
distributeTasksToNodes
  :: forall a k . (IsTask a, Ord k)
  => Config
  -> [Node]
  -> Set a
  -> (a -> k)
  -> (Map Node (CPUAllocation a), [a])
distributeTasksToNodes config nodes taskSet taskPriority =
  go sortedTaskList (Map.fromList [(n, Map.empty) | n <- nodes])
  where
    -- Sort from largest to lowest priority
    sortedTaskList = List.sortOn (Down . taskPriority) $ Set.toList taskSet

    go :: [a] -> Map Node (CPUAllocation a) -> (Map Node (CPUAllocation a), [a])
    go []                assignments = (assignments, [])
    go ts@(task : tasks) assignments =
      let
        candidateNodes =
          List.sortOn (allocCompletionTime . snd) $
          filter (canHandleTask config task) $
          Map.toList assignments
      in case candidateNodes of
        [] -> (assignments, ts)
        (node, oldAlloc) : _ ->
          let
            newAlloc = allocateCpusToTasks InitialRun node.cpus (task : Map.keys oldAlloc)
          in
            go tasks (Map.insert node newAlloc assignments)

{-

This section includes a heuristic that seems to help with the initial
allocation of tasks at the beginning, but I do not know how to use it
when things are running. The heuristic is to assign a score to an
allocation of resources as follows. Occupied CPUs are used with
efficiency 1. We assume that k free CPUs are used with efficiency
e(k), where e(k) is an increasing function of k, and e(N) < 1, where N
is the number of CPUs on the machine. The idea is that we will try our
best to pack tasks into free CPUs. When there are fewer free CPUs
(smaller k), it is harder to pack tasks into them efficiently, hence
e(k) should be increasing. We should have e(N) < 1 because no matter
how good our algorithm is, it won't be able to get perfect efficiency
from free CPUs. Now the "raw score" of an allocation with runtime T is

\int_0^T dt (N-k(t)) + k(t) e(k(t))

i.e. it is the amount of computation it does, with the given
efficiency estimates. The normalized score of an allocation is its raw
score minus e(N) T.

Some examples:

- an empty allocation, with N free CPUs lasting for any arbitrary time
  T has score 0.

- a full allocation with 0 free CPUs for time T, and N free CPUs
  afterwards has score NT(1-e(N))

- an allocation where the CPUs don't all finish at the same time has a
  smaller score than one that does an equivalent amount of computation
  but the CPUs do finish at the same time.

In practice, I chose e(k) = (k/N)*e_0, where e_0 = 0.75.

To use this algorithm, one can replace

distributeTasksToNodes nodes (TaskGraph.independentKeys taskGraph)
==>
distributeTasksToNodesWithScores 0.75 nodes (TaskGraph.independentKeys taskGraph)

in RunTasks.hs. It does seem to help with the problem of small tasks
squeezing CPUs away from big tasks, at the beginning. But I do not
know how to use this heuristic effectively during a run.

-}


partialSums :: Num a => [a] -> [a]
partialSums = drop 1 . scanl (+) 0

differences :: Num a => [a] -> [a]
differences xs = zipWith (-) xs (drop 1 xs)

integrateAlloc
  :: (IsTask a, Real b)
  => (NumCPUs -> b)
  -> CPUAllocation a
  -> NominalDiffTime
integrateAlloc f alloc = sum $ zipWith (*) times weights
  where
    sortedTasks = List.sortOn (negate . getRuntime) (Map.toList alloc)
    usedCpuCounts = partialSums (map snd sortedTasks)
    weights = map (realToFrac . f) usedCpuCounts
    times = differences (map getRuntime sortedTasks <> [0])

allocScore :: IsTask a => Double -> Node -> CPUAllocation a -> NominalDiffTime
allocScore e0 node = integrateAlloc integrand
  where
    totalCpus = fromIntegral node.cpus
    efficiency freeCpus = e0 * freeCpus / totalCpus
    relativeComputationRate freeCpus =
      totalCpus * (1 - efficiency totalCpus) - freeCpus * (1 - efficiency freeCpus)
    integrand inUseCpus = relativeComputationRate (totalCpus - fromIntegral inUseCpus)

-- | For each task, choose the node that can handle the task with the
-- lowest completion time and assign that task to that
-- node. Reallocate cpus on the node to optimize the completion
-- time. Return the node assignments and a list of any un-assigned
-- tasks.
--
-- TODO: Currently, we choose the node that will finish first,
-- before we add the next task. However, perhaps we should consider
-- adding the task to each node and see how long it will take to
-- finish. If we don't take that strategy, then we probably shouldn't
-- re-compute the completion time every round.
distributeTasksToNodesWithScores
  :: forall a k . (IsTask a, Ord k)
  => Double
  -> Config
  -> [Node]
  -> Set a
  -> (a -> k)
  -> (Map Node (CPUAllocation a), [a])
distributeTasksToNodesWithScores e0 config nodes taskSet taskPriority =
  case (sortedTaskList, nodes) of
    ([], _)     -> (Map.empty, [])
    (tasks, []) -> (Map.empty, tasks)
    (task0 : _, node0 : _)
      | canHandleTask config task0 (node0, Map.empty) ->
        go sortedTaskList (Map.fromList [(n, Map.empty) | n <- nodes])
      | otherwise ->
        error $ concat
        [ "First node cannot handle first task"
        , ": taskMemory: ", show (taskMemoryCapped node0.memory task0)
        , ", taskMinThreads: ", show (taskMinThreads InitialRun task0)
        , ", taskLocalFileSize: ", show (sum $ Map.elems $ taskLocalFileSizeMap config task0)
        , ", node: ", show node0
        ]
  where
    -- Sort from largest to lowest priority
    sortedTaskList = List.sortOn (Down . taskPriority) $ Set.toList taskSet

    taskIncreasesScore :: a -> (Node, CPUAllocation a) -> Maybe (Node, CPUAllocation a)
    taskIncreasesScore task (node, oldAlloc) =
      -- We want to fill all CPUs even if it doesn't increase the score.
      if newMaxTotalCpus <= node.cpus || newScore > oldScore
      then Just (node, newAlloc)
      else Nothing
      where
        newAlloc = allocateCpusToTasks InitialRun node.cpus (task : Map.keys oldAlloc)
        oldScore = allocScore e0 node oldAlloc
        newScore = allocScore e0 node newAlloc
        -- Max number of CPUs that can be filled with the new allocation
        newMaxTotalCpus = sum $ map (taskMaxThreads InitialRun) $ Map.keys newAlloc

    bestNodeForTask :: a -> Map Node (CPUAllocation a) -> Maybe (Node, CPUAllocation a)
    bestNodeForTask task assignments = case goodNodes of
      newNodeAlloc : _ -> Just newNodeAlloc
      []               -> Nothing
      where
        goodNodes =
          List.sortOn (allocCompletionTime . snd) $
          mapMaybe (taskIncreasesScore task) $
          filter (canHandleTask config task) $
          Map.toList assignments

    go :: [a] -> Map Node (CPUAllocation a) -> (Map Node (CPUAllocation a), [a])
    go []                assignments = (assignments, [])
    go ts@(task : tasks) assignments
      | Just (node, newAlloc) <- bestNodeForTask task assignments =
          go tasks (Map.insert node newAlloc assignments)
      | otherwise = (assignments, ts)
