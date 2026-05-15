{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Hyperion.Scheduler.TaskInfo where

import Data.Set                           (Set)
import Data.Set                           qualified as Set
import Data.Text                          (Text)
import Data.Time.Clock                    (NominalDiffTime)
import Debug.Trace                        qualified as Debug
import Hyperion.Scheduler.FilePath        (VirtualFilePath)
import Hyperion.Scheduler.TaskKeyFileInfo (TaskKeyFileInfo (..))
import Hyperion.Scheduler.Types           (MemorySize, NumCPUs)

type Tag = Text

-- | We allow minThreads and maxThreads to depend on the stage of the
-- computation. TODO: Really, minThreads and maxThreads should be able
-- to depend on stats. How do we achieve that?
data RunStage = InitialRun | InProgressRun

data TaskInfo = MkTaskInfo
  { -- | Estimated memory in bytes
    memory     :: MemorySize
    -- | Estimated runtime in seconds, as a function of NumCPUs
  , runtime    :: NumCPUs -> NominalDiffTime
    -- | Maximum possible threads for the task
  , maxThreads :: RunStage -> NumCPUs
    -- | Minimum possible threads for the task
  , minThreads :: RunStage -> NumCPUs
    -- | A label indicating the type of task. If Nothing, the task
    -- will be ommitted from progress reports.
    -- List of input files
  , inputs     :: Set TaskKeyFileInfo
    -- List of output files
  , outputs    :: Set TaskKeyFileInfo
  , priority   :: Int
  , tag        :: Maybe Tag
  }

class HasTaskInfo a where
  taskInfo :: a -> TaskInfo

-- | Unit is sometimes useful as a top level placeholder task.
instance HasTaskInfo () where
  taskInfo _ = emptyTaskInfo

emptyTaskInfo :: TaskInfo
emptyTaskInfo = MkTaskInfo
    { memory     = 0
    , runtime    = const 0
    , maxThreads = const 0
    , minThreads = const 0
    , inputs     = Set.empty
    , outputs    = Set.empty
    , priority   = 0
    , tag        = Nothing
    }

taskMemory :: HasTaskInfo a => a -> MemorySize
taskMemory t = (taskInfo t).memory

-- Returns min(maxMemory, taskMemory t)
taskMemoryCapped :: HasTaskInfo a => MemorySize -> a -> MemorySize
taskMemoryCapped maxMemory t =
  let
    memEstimate = (taskInfo t).memory
  in
    if memEstimate > maxMemory
    then Debug.trace (concat
                       [ "WARNING: task memEstimate exceeds maxMemory."
                       , " Replacing it with maxMemory. This might lead to a crash."
                       , " memEstimate = ", show memEstimate
                       , ", maxMemory = ", show maxMemory
                       , ", task.tag = ", show (taskInfo t).tag
                       ]) maxMemory
    else memEstimate

taskRuntime :: HasTaskInfo a => a -> NumCPUs -> NominalDiffTime
taskRuntime t = (taskInfo t).runtime

taskMinThreads :: HasTaskInfo a => RunStage -> a -> NumCPUs
taskMinThreads stage t = (taskInfo t).minThreads stage

taskMaxThreads :: HasTaskInfo a => RunStage -> a -> NumCPUs
taskMaxThreads stage t = (taskInfo t).maxThreads stage

taskInputs :: HasTaskInfo a => a -> Set TaskKeyFileInfo
taskInputs t = (taskInfo t).inputs

taskOutputs :: HasTaskInfo a => a -> Set TaskKeyFileInfo
taskOutputs t = (taskInfo t).outputs

taskInputPaths :: HasTaskInfo a => a -> Set VirtualFilePath
taskInputPaths t = Set.map (.path) (taskInfo t).inputs

taskOutputPaths :: HasTaskInfo a => a -> Set VirtualFilePath
taskOutputPaths t = Set.map (.path) (taskInfo t).outputs

defaultTaskPriority :: HasTaskInfo a => a -> Int
defaultTaskPriority t = (taskInfo t).priority

class ToMemoryEstimate a where
  toMemoryEstimate :: a -> MemorySize

instance ToMemoryEstimate TaskInfo where
  toMemoryEstimate = (.memory)


-- If you don't have a better way to estimate runtime of your task, try this one.
-- It produces reasonably-looking times.
-- The constant 1.7e-6 originally came from our blocks_3d tests on Expanse.
memoryToCpuTimeApprox :: MemorySize -> NominalDiffTime
memoryToCpuTimeApprox mem = 1.7e-6 * fromIntegral mem
