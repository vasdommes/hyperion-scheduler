{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoFieldSelectors          #-}
{-# LANGUAGE OverloadedRecordDot       #-}
{-# LANGUAGE StaticPointers            #-}
{-# LANGUAGE TypeFamilies              #-}

module Hyperion.Scheduler.Task
  ( Task
  , mkTask
  , mkTaskWithStats
  ) where

import Data.Aeson                         (ToJSON (..))
import Data.Binary                        (Binary (..))
import Data.BinaryHash                    (hashBase64SafeByteString)
import Data.ByteString                    (ByteString)
import Data.Maybe                         (fromMaybe)
import Data.Set                           (Set)
import Data.Set                           qualified as Set
import Data.Time (NominalDiffTime)
import Hyperion.Scheduler.IsTask (IsTask (..))
import Hyperion.Scheduler.Stats           (TaskAndFileStats, ToStatKey (..),
                                           approxRuntime, lookupMaxFileSize,
                                           lookupTaskStats, maxMemory)
import Hyperion.Scheduler.TaskKeyFileInfo (TaskKeyFileInfo (..))
import Hyperion.Scheduler.Types (MemorySize (..), NumCPUs)

-- | A general container for an instance of IsTask and
-- CanRemoteRunTask. We include a ByteString hash for quick
-- comparisons, so that data structures like Set's and Map's are
-- reasonably performant.
--
-- WARNING: If two different tasks ever have the same hash, this could
-- lead to undefined behavior.
data Task = forall a . (IsTask a, ToStatKey a) => MkTask
  { task            :: a
  -- Cache some values computed from task
  , hash            :: ByteString
  , inputs          :: Set TaskKeyFileInfo
  , outputs         :: Set TaskKeyFileInfo
  -- Memory and runtime estimates can come from task or be overriden by stats
  , memoryEstimate  :: MemorySize
  , runtimeEstimate :: NumCPUs -> NominalDiffTime
  }


instance Eq Task where
  x == y = x.hash == y.hash

instance Ord Task where
  compare x y = compare x.hash y.hash

--instance Show Task where
--  show (MkTask h i _) = concat
--    [ "MkTask "
--    , case i.tag of
--        Nothing -> "Nothing"
--        Just t' -> Text.unpack t'
--    , " "
--    , show (i.runtime 1)
--    , " \""
--    , ByteString.unpack h
--    , "\""
--    ]

instance ToJSON Task where
  toJSON (MkTask {task = t}) = toJSON t

instance IsTask Task where
  taskMemoryEstimate t = t.memoryEstimate
  taskRuntimeEstimate t = t.runtimeEstimate
  taskMaxThreads stage (MkTask { task = t }) = taskMaxThreads stage t
  taskMinThreads stage (MkTask { task = t }) = taskMinThreads stage t
  taskInputs t = t.inputs
  taskOutputs t = t.outputs
  taskDefaultPriority (MkTask { task = t }) = taskDefaultPriority t
  taskTag (MkTask { task = t }) = taskTag t
  taskClosure numCpus (MkTask { task = t }) = taskClosure numCpus t

instance ToStatKey Task where
  toStatKey (MkTask { task = t }) = toStatKey t


-- | A smart constructor for a Task.
mkTask :: (IsTask a, ToStatKey a, Binary a) => a -> Task
mkTask t = MkTask
  { task = t
  , hash = hashBase64SafeByteString t
  , inputs = taskInputs t
  , outputs = taskOutputs t
  , memoryEstimate = taskMemoryEstimate t
  , runtimeEstimate = taskRuntimeEstimate t
  }

-- | Create a task whose memory and runtime are estimted with the
-- given 'TaskResourceMap's.
mkTaskWithStats
  :: (IsTask a, ToStatKey a, Binary a)
  => TaskAndFileStats
  -> a
  -> Task
mkTaskWithStats stats task = (mkTask task)
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
