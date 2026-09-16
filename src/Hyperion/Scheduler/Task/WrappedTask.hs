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
import Data.ByteString.Lazy           qualified as Lazy
import Data.Map.Strict                (Map)
import Data.Maybe                     (fromMaybe)
import Data.Set                       (Set)
import Data.Set                       qualified as Set
import Data.Time                      (NominalDiffTime)
import Hyperion.Scheduler.StatKey     (TaskKeyFileInfo (..))
import Hyperion.Scheduler.Stats       (TaskAndFileStats, ToStatKey (..),
                                       approxRuntime, lookupMaxFileSize,
                                       lookupTaskStats, maxMemory)
import Hyperion.Scheduler.Task.IsTask (IsTask (..))
import Hyperion.Scheduler.Types       (MemorySize (..), NumCPUs)

-- | A general container for an instance of IsTask and
-- CanRemoteRunTask. We include a ByteString hash for quick
-- comparisons, so that data structures like Set's and Map's are
-- reasonably performant.
--
-- WARNING: If two different tasks ever have the same hash, this could
-- lead to undefined behavior.
data WrappedTask = forall a . (IsTask a, ToStatKey a) => MkWrappedTask
  { task            :: a
  -- Cache some values computed from task
  , hash            :: ByteString
  , inputs          :: Set TaskKeyFileInfo
  , outputs         :: Set TaskKeyFileInfo
  -- Memory and runtime estimates can come from task or be overriden by stats
  , memoryEstimate  :: MemorySize
  , runtimeEstimate :: NumCPUs -> NominalDiffTime
  -- | How to build the task's follow-ups, see 'taskFollowUps'. Not part of
  -- the task's identity: comparisons and serialisation look at 'task' only.
  , followUps       :: Maybe FollowUpBuilder
  }

-- | Builds the tasks a running task adds to its run, from the encoded
-- follow-up it sends ("Hyperion.Scheduler.Task.FollowUps").
type FollowUpBuilder = Lazy.ByteString -> IO (Map WrappedTask (Set WrappedTask))


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
  taskKeepOutputs (MkWrappedTask { task = t }) = taskKeepOutputs t
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
  taskClosureWithHandle numCpus handle (MkWrappedTask { task = t }) = taskClosureWithHandle numCpus handle t
  taskFollowUps t = t.followUps

instance ToStatKey WrappedTask where
  toStatKey (MkWrappedTask { task = t }) = toStatKey t


-- | A smart constructor for a WrappedTask.
wrapTask :: (IsTask a, ToStatKey a, Binary a) => a -> WrappedTask
wrapTask = wrapTaskWithFollowUps Nothing

-- | 'wrapTask' for a task that may add follow-ups to its run: the builder is
-- made by whoever knows the task's resolver and configs (the task chain, see
-- "Hyperion.Scheduler.Task.Task").
wrapTaskWithFollowUps :: (IsTask a, ToStatKey a, Binary a) => Maybe FollowUpBuilder -> a -> WrappedTask
wrapTaskWithFollowUps builder t = MkWrappedTask
  { task = t
  , hash = hashBase64SafeByteString t
  , inputs = taskInputs t
  , outputs = taskOutputs t
  , memoryEstimate = taskMemoryEstimate t
  , runtimeEstimate = taskRuntimeEstimate t
  , followUps = builder
  }

-- | Update memory, runtime and file size estimates using statistics from TaskAndFileStats.
decorateTaskWithStats :: TaskAndFileStats -> WrappedTask -> WrappedTask
decorateTaskWithStats stats task = task
  { memoryEstimate = memory
  , runtimeEstimate = runtime
  , inputs = inputs
  , outputs = outputs
  }
  where
    maybeTaskResourceMap = lookupTaskStats task stats
    runtime = fromMaybe (taskRuntimeEstimate task) (maybeTaskResourceMap >>= approxRuntime Nothing)
    memory  = fromMaybe (taskMemoryEstimate task)  (maybeTaskResourceMap >>= maxMemory)

    updateFileSize fileInfo = fileInfo { fileSize = fileSize} where
      fileSize = fromMaybe fileInfo.fileSize $ lookupMaxFileSize (fileInfo.fileStatKey) stats
    inputs = Set.map updateFileSize $ taskInputs task
    outputs = Set.map updateFileSize $ taskOutputs task

-- | Create a task whose memory and runtime are estimted with the
-- given 'TaskResourceMap's.
-- TODO: if a ~ WrappedTask, should we wrap it again or do nothing?
wrapTaskWithStats
  :: (IsTask a, ToStatKey a, Binary a)
  => TaskAndFileStats
  -> a
  -> WrappedTask
wrapTaskWithStats stats = decorateTaskWithStats stats . wrapTask
