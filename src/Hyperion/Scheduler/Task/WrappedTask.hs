{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoFieldSelectors          #-}
{-# LANGUAGE OverloadedRecordDot       #-}
{-# LANGUAGE StaticPointers            #-}
{-# LANGUAGE TypeFamilies              #-}

module Hyperion.Scheduler.Task.WrappedTask where

import Data.Aeson                     (ToJSON (..))
import Data.Binary                    (Binary (..))
import Data.BinaryHash                (hashBase64SafeByteString)
import Data.ByteString                (ByteString)
import Data.Set                       (Set)
import Data.Set                       qualified as Set
import Data.Time                      (NominalDiffTime)
import Hyperion.Scheduler.StatKey     (StatKey, TaskKeyFileInfo (..))
import Hyperion.Scheduler.Stats       (TaskAndFileStats, approxRuntime,
                                       lookupMaxFileSize, lookupTaskStats,
                                       maxMemory)
import Hyperion.Scheduler.Task.IsTask (IsTask (..), ResourceEstimates (..))
import Hyperion.Scheduler.Types       (Estimate, MemorySize (..), NumCPUs,
                                       overrideWithMeasured)

-- | A general container for an instance of IsTask and
-- CanRemoteRunTask. We include a ByteString hash for quick
-- comparisons, so that data structures like Set's and Map's are
-- reasonably performant.
--
-- WARNING: If two different tasks ever have the same hash, this could
-- lead to undefined behavior.
data WrappedTask = forall a . IsTask a => MkWrappedTask
  { task            :: a
  -- Cache some values computed from task
  , hash            :: ByteString
  , inputs          :: Set TaskKeyFileInfo
  , outputs         :: Set TaskKeyFileInfo
  -- Computed once here, where the task's config is still in hand, so that
  -- nothing on the runtime path has to rebuild it. 'Nothing' for tasks with
  -- no identity in statistics -- see 'taskStatKey'.
  , statKey         :: Maybe StatKey
  -- Memory and runtime estimates can come from task or be overriden by stats.
  -- Each remembers which it was, so that 'runTasks' can report how much of the
  -- map is running on measurements rather than guesses without needing the
  -- statistics itself, and so that a task record can state what the task's own
  -- model predicted alongside what actually happened.
  , memoryEstimate  :: Estimate MemorySize
  , runtimeEstimate :: Estimate (NumCPUs -> NominalDiffTime)
  }

instance Eq WrappedTask where
  x == y = x.hash == y.hash

instance Ord WrappedTask where
  compare x y = compare x.hash y.hash

--instance Show WrappedTask where
--  show (MkWrappedTask h i _) = concat
--    [ "MkWrappedTask "
--    , case i.tag of
--        Nothing -> "Nothing"
--        Just t' -> Text.unpack t'
--    , " "
--    , show (i.runtime 1)
--    , " \""
--    , ByteString.unpack h
--    , "\""
--    ]

instance ToJSON WrappedTask where
  toJSON (MkWrappedTask {task = t}) = toJSON t

instance IsTask WrappedTask where
  taskMaxThreads stage (MkWrappedTask { task = t }) = taskMaxThreads stage t
  taskMinThreads stage (MkWrappedTask { task = t }) = taskMinThreads stage t
  taskInputs t = t.inputs
  taskOutputs t = t.outputs
  taskDefaultPriority (MkWrappedTask { task = t }) = taskDefaultPriority t
  taskTag (MkWrappedTask { task = t }) = taskTag t
  taskClosure numCpus (MkWrappedTask { task = t }) = taskClosure numCpus t
  taskIsPlaceholder (MkWrappedTask { task = t }) = taskIsPlaceholder t
  taskPlaceholderKey (MkWrappedTask { task = t }) = taskPlaceholderKey t
  taskStatKey t = t.statKey
  taskResourceEstimates t = MkResourceEstimates
    { memory  = t.memoryEstimate
    , runtime = t.runtimeEstimate
    }

-- | A smart constructor for a WrappedTask.
--
-- The estimates are taken whole from the task, rather than rebuilt from
-- 'taskMemoryEstimate' as 'EstimatedByTask': for a task using the default
-- 'taskResourceEstimates' the two are the same thing, but a task already
-- carrying measurements keeps them, instead of having a measured figure
-- relabelled as its own prediction.
wrapTask :: (IsTask a, Binary a) => a -> WrappedTask
wrapTask t = MkWrappedTask
  { task = t
  , hash = hashBase64SafeByteString t
  , inputs = taskInputs t
  , outputs = taskOutputs t
  , statKey = taskStatKey t
  , memoryEstimate = estimates.memory
  , runtimeEstimate = estimates.runtime
  }
  where
    estimates = taskResourceEstimates t

-- | Update memory, runtime and file size estimates using statistics from TaskAndFileStats.
--
-- Lookup is an exact match on the stat key, so a task whose key has changed
-- (a new estimate-relevant config value, say) misses and keeps its analytic
-- estimate; a task with no stat key is never looked up at all. A miss is not
-- reported here -- this function is pure, and its callers have no 'MonadIO' --
-- but it is visible in the resulting 'Estimate's, which 'runTasks' reports.
--
-- Memory and runtime are replaced independently: memory statistics are absent
-- whenever no run recorded a memory figure, while runtime statistics are
-- always recorded, so a task can end up running on a measured runtime and its
-- own memory estimate.
decorateTaskWithStats :: TaskAndFileStats -> WrappedTask -> WrappedTask
decorateTaskWithStats stats task = task
  { memoryEstimate = maybe id overrideWithMeasured measuredMemory task.memoryEstimate
  , runtimeEstimate = maybe id overrideWithMeasured measuredRuntime task.runtimeEstimate
  , inputs = inputs
  , outputs = outputs
  }
  where
    maybeTaskResourceMap = flip lookupTaskStats stats =<< task.statKey
    measuredRuntime = maybeTaskResourceMap >>= approxRuntime Nothing
    measuredMemory  = maybeTaskResourceMap >>= maxMemory

    -- A file with no stat key is never looked up and keeps its estimate.
    updateFileSize fileInfo = fileInfo { fileSize = fileSize } where
      fileSize = maybe id overrideWithMeasured measured fileInfo.fileSize
      measured = flip lookupMaxFileSize stats =<< fileInfo.fileStatKey
    inputs = Set.map updateFileSize $ taskInputs task
    outputs = Set.map updateFileSize $ taskOutputs task

-- | Create a task whose memory and runtime are estimted with the
-- given 'TaskResourceMap's.
-- TODO: if a ~ WrappedTask, should we wrap it again or do nothing?
wrapTaskWithStats
  :: (IsTask a, Binary a)
  => TaskAndFileStats
  -> a
  -> WrappedTask
wrapTaskWithStats stats = decorateTaskWithStats stats . wrapTask
