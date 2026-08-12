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

instance ToStatKey WrappedTask where
  toStatKey (MkWrappedTask { task = t }) = toStatKey t


-- | A smart constructor for a WrappedTask.
wrapTask :: (IsTask a, ToStatKey a, Binary a) => a -> WrappedTask
wrapTask t = MkWrappedTask
  { task = t
  , hash = hashBase64SafeByteString t
  , inputs = taskInputs t
  , outputs = taskOutputs t
  , memoryEstimate = taskMemoryEstimate t
  , runtimeEstimate = taskRuntimeEstimate t
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
