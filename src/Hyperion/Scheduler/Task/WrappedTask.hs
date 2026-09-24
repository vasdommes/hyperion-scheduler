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
import Data.Maybe                     (fromMaybe)
import Data.Set                       (Set)
import Data.Set                       qualified as Set
import Data.Time                      (NominalDiffTime)
import Hyperion.Scheduler.StatKey     (StatKey, TaskKeyFileInfo (..))
import Hyperion.Scheduler.Stats       (TaskAndFileStats, approxRuntime,
                                       lookupMaxFileSize, lookupTaskStats,
                                       maxMemory)
import Hyperion.Scheduler.Task.IsTask (EstimateSource (..), IsTask (..))
import Hyperion.Scheduler.Types       (MemorySize (..), NumCPUs)

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
  -- Memory and runtime estimates can come from task or be overriden by stats
  , memoryEstimate  :: MemorySize
  , runtimeEstimate :: NumCPUs -> NominalDiffTime
  -- Which of those two it was. Recorded so that 'runTasks' can report how much
  -- of the map is running on measurements rather than guesses, without needing
  -- the statistics itself.
  , estimateSource  :: EstimateSource
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
  taskMemoryEstimate t = t.memoryEstimate
  taskRuntimeEstimate t = t.runtimeEstimate
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
  taskEstimateSource t = t.estimateSource

-- | A smart constructor for a WrappedTask.
wrapTask :: (IsTask a, Binary a) => a -> WrappedTask
wrapTask t = MkWrappedTask
  { task = t
  , hash = hashBase64SafeByteString t
  , inputs = taskInputs t
  , outputs = taskOutputs t
  , statKey = taskStatKey t
  , memoryEstimate = taskMemoryEstimate t
  , runtimeEstimate = taskRuntimeEstimate t
  , estimateSource = EstimatedByTask
  }

-- | Update memory, runtime and file size estimates using statistics from TaskAndFileStats.
--
-- Lookup is an exact match on the stat key, so a task whose key has changed
-- (a new estimate-relevant config value, say) misses and keeps its analytic
-- estimate; a task with no stat key is never looked up at all. A miss is not
-- reported here -- this function is pure, and its callers have no 'MonadIO' --
-- but it is recorded in 'estimateSource', which 'runTasks' reports.
decorateTaskWithStats :: TaskAndFileStats -> WrappedTask -> WrappedTask
decorateTaskWithStats stats task = task
  { memoryEstimate = memory
  , runtimeEstimate = runtime
  , inputs = inputs
  , outputs = outputs
  , estimateSource = source
  }
  where
    maybeTaskResourceMap = flip lookupTaskStats stats =<< task.statKey
    measuredRuntime = maybeTaskResourceMap >>= approxRuntime Nothing
    measuredMemory  = maybeTaskResourceMap >>= maxMemory

    runtime = fromMaybe (taskRuntimeEstimate task) measuredRuntime
    memory  = fromMaybe (taskMemoryEstimate task)  measuredMemory

    -- Keep what the task itself predicted, but only when something replaced it.
    source = case measuredMemory of
      Nothing -> EstimatedByTask
      Just _  -> MeasuredFromStats (taskMemoryEstimate task)

    -- A file with no stat key is never looked up and keeps its estimate.
    updateFileSize fileInfo = fileInfo { fileSize = fileSize} where
      fileSize = fromMaybe fileInfo.fileSize $
        flip lookupMaxFileSize stats =<< fileInfo.fileStatKey
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
