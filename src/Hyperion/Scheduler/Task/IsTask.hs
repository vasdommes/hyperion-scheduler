{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Hyperion.Scheduler.Task.IsTask where

import Data.Aeson                  (ToJSON)
import Data.Set                    (Set)
import Data.Set                    qualified as Set
import Data.Text                   (Text)
import Data.Text                   qualified as Text
import Data.Time.Clock             (NominalDiffTime)
import Data.Typeable               (Typeable, typeOf)
import Debug.Trace                 qualified as Debug
import Hyperion                    (Closure, Process)
import Hyperion.Scheduler.FilePath (VirtualFilePath)
import Hyperion.Scheduler.StatKey  (TaskKeyFileInfo (..))
import Hyperion.Scheduler.Types    (MemorySize, NumCPUs)

-- | We allow minThreads and maxThreads to depend on the stage of the
-- computation. TODO: Really, minThreads and maxThreads should be able
-- to depend on stats. How do we achieve that?
data RunStage = InitialRun | InProgressRun

type Tag = Text

-- Everything Scheduler needs
-- HasTaskHash + IsTask + CanRemoteRunTask + ToStatKey
class (Typeable a, ToJSON a, Eq a, Ord a) => IsTask a where
--  taskHash :: a -> ByteString
--  default taskHash :: Binary a => a -> ByteString
--  taskHash = hashBase64SafeByteString

  -- | Estimated memory in bytes
  taskMemoryEstimate     :: a -> MemorySize
  taskMemoryEstimate = const 0
  -- | Estimated runtime in seconds, as a function of NumCPUs
  taskRuntimeEstimate    :: a -> NumCPUs -> NominalDiffTime
  taskRuntimeEstimate t numCpus = memoryToCpuTimeApprox (taskMemoryEstimate t) / fromIntegral numCpus
  -- | Maximum possible threads for the task
  -- TODO: get rid of RunStage?
  taskMaxThreads :: RunStage -> a -> NumCPUs
  taskMaxThreads _ _ = 1
  -- | Minimum possible threads for the task
  taskMinThreads :: RunStage -> a -> NumCPUs
  taskMinThreads _ _ = 1
  -- | A label indicating the type of task. If Nothing, the task
  -- will be ommitted from progress reports.
  -- List of input files
  taskInputs     :: a -> Set TaskKeyFileInfo
  -- List of output files
  taskOutputs    :: a -> Set TaskKeyFileInfo

  taskDefaultPriority   :: a -> Int
  taskDefaultPriority = const 0

  taskTag        :: a -> Maybe Tag
  taskTag = Just . Text.pack . show . typeOf

  taskClosure :: NumCPUs -> a -> Maybe (Closure (Process ()))

-- TODO: remove (Stats.ToStatKey a) and use (IsTask a) everywhere in Stats instead?
--  taskStatKey :: a -> StatKey
--  -- | A default implementation for the case where we wish to retain
--  -- all the information about a task in the StatKey.
--  taskStatKey = mkStatKeyViaJSON

-- If you don't have a better way to estimate runtime of your task, try this one.
-- It produces reasonably-looking times.
-- The constant 1.7e-6 originally came from our blocks_3d tests on Expanse.
memoryToCpuTimeApprox :: MemorySize -> NominalDiffTime
memoryToCpuTimeApprox mem = 1.7e-6 * fromIntegral mem

-- Returns min(maxMemory, taskMemory t)
taskMemoryCapped :: IsTask a => MemorySize -> a -> MemorySize
taskMemoryCapped maxMemory t =
  let
    memEstimate = taskMemoryEstimate t
  in
    if memEstimate > maxMemory
    then Debug.trace (concat
                       [ "WARNING: task memEstimate exceeds maxMemory."
                       , " Replacing it with maxMemory. This might lead to a crash."
                       , " memEstimate = ", show memEstimate
                       , ", maxMemory = ", show maxMemory
                       , ", task.tag = ", show (taskTag t)
                       ]) maxMemory
    else memEstimate

taskInputPaths :: IsTask a => a -> Set VirtualFilePath
taskInputPaths t = Set.map (.path) $ taskInputs t

taskOutputPaths :: IsTask a => a -> Set VirtualFilePath
taskOutputPaths t = Set.map (.path) $ taskOutputs t
