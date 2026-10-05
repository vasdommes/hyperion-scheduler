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
import Data.Set                       qualified as Set
import Hyperion.Scheduler.StatKey     (TaskKeyFileInfo (..))
import Hyperion.Scheduler.Stats       (TaskAndFileStats, approxRuntime,
                                       lookupMaxFileSize, lookupTaskStats,
                                       maxMemory)
import Hyperion.Scheduler.Task.IsTask (IsTask (..), ResourceEstimates (..),
                                       TaskSummary (..))
import Hyperion.Scheduler.Types       (overrideWithMeasured)

-- | A general container for an instance of IsTask and
-- CanRemoteRunTask. We include a ByteString hash for quick
-- comparisons, so that data structures like Set's and Map's are
-- reasonably performant.
--
-- WARNING: If two different tasks ever have the same hash, this could
-- lead to undefined behavior.
data WrappedTask = forall a . IsTask a => MkWrappedTask
  { task    :: a
  -- Cache some values computed from task
  , hash    :: ByteString
  -- Computed once here, where the task's config is still in hand, so that
  -- nothing on the runtime path has to rebuild it. Its estimates can come
  -- from the task or be overriden by stats. Each remembers which it was, so
  -- that a task record can state what the task's own model predicted
  -- alongside what actually happened.
  , summary :: TaskSummary
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
  taskSummary Nothing t = t.summary
  taskSummary knownInputs (MkWrappedTask { task = t }) = taskSummary knownInputs t
  taskMaxThreads stage (MkWrappedTask { task = t }) = taskMaxThreads stage t
  taskMinThreads stage (MkWrappedTask { task = t }) = taskMinThreads stage t
  taskDefaultPriority (MkWrappedTask { task = t }) = taskDefaultPriority t
  taskTag (MkWrappedTask { task = t }) = taskTag t
  taskClosure (MkWrappedTask { task = t }) = taskClosure t
  taskIsPlaceholder (MkWrappedTask { task = t }) = taskIsPlaceholder t
  taskPlaceholderKey (MkWrappedTask { task = t }) = taskPlaceholderKey t

-- | A smart constructor for a WrappedTask.
--
-- The estimates are taken whole from the task's summary, so a task already
-- carrying measurements keeps them, instead of having a measured figure
-- relabelled as its own prediction.
wrapTask :: (IsTask a, Binary a) => a -> WrappedTask
wrapTask t = MkWrappedTask
  { task    = t
  , hash    = hashBase64SafeByteString t
  , summary = taskSummary Nothing t
  }

-- | Update memory, runtime and output file size estimates using statistics
-- from TaskAndFileStats. Input files keep their sizes: those come from the
-- tasks producing them, already decorated, or from the disk.
--
-- Lookup is an exact match on the stat key, so a task whose key has changed
-- (a new estimate-relevant config value, say) misses and keeps its analytic
-- estimate; a task with no stat key is never looked up at all. A miss shows
-- in the resulting 'Estimate's.
--
-- Memory and runtime are replaced independently: memory statistics are absent
-- whenever no run recorded a memory figure, while runtime statistics are
-- always recorded, so a task can end up running on a measured runtime and its
-- own memory estimate.
decorateSummaryWithStats :: TaskAndFileStats -> TaskSummary -> TaskSummary
decorateSummaryWithStats stats summary = summary
  { outputs   = Set.map decorateFile summary.outputs
  , estimates = MkResourceEstimates
      { memory  = maybe id overrideWithMeasured measuredMemory summary.estimates.memory
      , runtime = maybe id overrideWithMeasured measuredRuntime summary.estimates.runtime
      }
  }
  where
    recorded = flip lookupTaskStats stats =<< summary.statKey
    measuredRuntime = recorded >>= approxRuntime Nothing
    measuredMemory  = recorded >>= maxMemory

    -- A file with no stat key is never looked up and keeps its estimate.
    decorateFile info = info { fileSize = fileSize } where
      fileSize = maybe id overrideWithMeasured measured info.fileSize
      measured = flip lookupMaxFileSize stats =<< info.fileStatKey

-- | 'decorateSummaryWithStats' for a wrapped task.
decorateTaskWithStats :: TaskAndFileStats -> WrappedTask -> WrappedTask
decorateTaskWithStats stats task = task { summary = decorateSummaryWithStats stats task.summary }
