{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StaticPointers        #-}

module Hyperion.Scheduler.RunTasks.TaskPriority
  ( TaskPriority
  , taskPriority
  , mkTaskPriorityHelper
  )
where

import Data.Array                     qualified as Array
import Data.Graph                     qualified as Graph
import Data.IntMap.Strict             (IntMap)
import Data.IntMap.Strict             qualified as IntMap
import Data.Time.Clock                (NominalDiffTime)
import Hyperion.Scheduler.Task.IsTask (IsTask (..), RunStage (..))
import Hyperion.Scheduler.TaskGraph   (TaskGraph, taskToVertex, vertexToTask)
import Hyperion.Scheduler.TaskGraph   qualified as TaskGraph
import Hyperion.Scheduler.Types       (MemorySize (..), Node (..))


-- CriticalPathPriority prioritizes the critical path of a task graph (the slowest dependency chain).
-- In addition, it prioritizes critical paths of task subtrees.
-- For a task with no reverse dependencies, it is just a time needed to build its critical path:
--     criticalPathPriority task = [criticalPathTime task]
-- For a task with reverse dependencies, CriticalPathPriority is its criticalPathTime appended
-- to maximal CriticalPathPriority of reverse dependencies:
--     criticalPathPriority task = max (map criticalPriority $ revDeps task) ++ [criticalPathTime task]
--
-- Example:
-- Consider the following dependency graph, where numbers denote runtime estimates for each task:
--   A(1)  <-- B(20)  <-- C(40)
--   A'(9) <-- B'(10) <--/
--   D(5)  <-- E(6)
-- Then the tasks will be prioritized as follows:
--   criticalPathPriority A  = [t(ABC), t(AB), t(A)] = [70,21,1]
--   criticalPathPriority B  = [t(ABC), t(AB)] = [70,21]
--   criticalPathPriority A' = [t(ABC), t(A'B'), t(A')] = [70,19,9]
--   criticalPathPriority B' = [t(ABC), t(A'B')] = [70,19]
--   criticalPathPriority C  = [t(ABC)] = [70]
--   criticalPathPriority D  = [t(DE), t(D)] = [11,5]
--   criticalPathPriority E  = [t(DE)] = [11]
-- where t(XYZ) = t(X) + t(Y) + t(Z) is a runtime estimate for a sequence of tasks XYZ.
-- This choice of priorities ensures that the critical path ABC is executed as fast as possible.
-- (Note that A and B have higher priority than A' and B'.)
--
-- This makes scheduling more DFS-like, whereas priorities based on TreeDepth are BFS-like.
-- (note, however, that it's not strictly DFS: if we have enough resources, we start A, A' and D immediately).
-- This behaviour should be better for local disk usage.
-- For example, BFS scheduler builds all Block3d files before proceeding to CompositeBlocks,
-- and potentially runs out of space.
-- DFS-like scheduler builds only Block3ds required for a particular CompositeBlock,
-- then builds this CompositeBlock and removes Block3d files.
-- This algorithm should work fine during the bulk of computation,
-- but what will happen in the end? We'll be building a chain af dependencies for low-priority task (E).
-- Hopefully, this tail won't be that long, since this task has the shortest critical path.
data CriticalPathPriority = CriticalPathPriority [NominalDiffTime]
  deriving (Eq, Ord, Show)


-- | Our choice of priority for tasks. It has three components:
-- 1. defaultPriority coming from taskInfo. Currently it's 0 for everything except CleanupTask (having priority = 100).
-- 2. CriticalPathPriority (see above)
-- 3. Task memory.
-- TODO: add extra field to ensure strict ordering?
type TaskPriority = (Int, CriticalPathPriority, MemorySize)

-- Extra data used to compute task priority
type TaskPriorityHelper a = (TaskGraph a, IntMap CriticalPathPriority)

mkTaskPriorityHelper :: (IsTask a) => [Node] -> TaskGraph a -> TaskPriorityHelper a
mkTaskPriorityHelper [] _    = error "Empty node list"
-- NB: here we assume that all nodes have the same memory and numCPUs (which is true in practice).
-- If not, we should maybe construct an "average node".
mkTaskPriorityHelper (node:_) taskGraph = (taskGraph, criticalPathPriorityMap) where

  -- For each task, compute build time for its critical path (the most expensive build chain).
  criticalPathTimeMap :: IntMap NominalDiffTime
  criticalPathTimeMap =
    -- Accumulate build time starting from independent tasks
    go (Graph.topSort taskGraph.revDependencyGraph) IntMap.empty
    where
      go [] m = m
      go (v : vs) m = go vs m' where
        m' = IntMap.insert v buildTime m
        buildTime = ownRuntime + maxDepsBuildTime

        maxDepsBuildTime = foldr max 0 $ map (m IntMap.! ) deps
        deps = taskGraph.dependencyGraph Array.! v

        -- Task runtime depends on number of CPUs.
        -- We don't know it in advance, so we try to come up with a realistic estimate.
        t = vertexToTask taskGraph v
        ownRuntime = taskRuntimeEstimate t expectedNumCpus
        expectedNumCpus =
          if minCpus == maxCpus then
            minCpus
          else
            max minCpus $ min maxCpus $ cpusFromMem
        minCpus = taskMinThreads runStage t
        maxCpus = min node.cpus $ taskMaxThreads runStage t
        runStage = if null deps then InitialRun else InProgressRun
        -- A fraction of node CPUs proportional to task memory.
        cpusFromMem = round $ (fromIntegral $ node.cpus * fromIntegral (taskMemoryEstimate t) :: Double) / fromIntegral node.memory

  criticalPathPriorityMap :: IntMap CriticalPathPriority
  criticalPathPriorityMap = IntMap.map (CriticalPathPriority . reverse) $
    -- Accumulate lists of critical path priorities starting from the latest tasks.
    go (Graph.topSort taskGraph.dependencyGraph) IntMap.empty
    where
      go [] m = m
      go (v:vs) m = go vs m' where
        m' = IntMap.insert v criticalTimes m
        -- NB: here ownCriticalTime becomes the first element.
        -- At the end we reverse the list to make it the last one.
        -- We do it since repeated appending may lead to poor performance. So we prepend and reverse instead.
        -- TODO: is it important? Shall we use Data.Sequence instead?
        criticalTimes = ownCriticalTime : revDepsCriticalTimes
        ownCriticalTime = criticalPathTimeMap IntMap.! v
        revDeps = taskGraph.revDependencyGraph Array.! v
        revDepsCriticalTimes = foldr max [] $ map (m IntMap.! ) revDeps

taskPriority :: IsTask a => TaskPriorityHelper a -> a -> TaskPriority
taskPriority (taskGraph, criticalPathPriorityMap) t = (taskDefaultPriority t, criticalPathPriority, taskMemoryEstimate t) where
  v = taskToVertex taskGraph t
  criticalPathPriority = criticalPathPriorityMap IntMap.! v
