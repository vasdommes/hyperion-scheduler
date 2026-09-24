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
import Hyperion.Scheduler.StatKey  (StatKey, TaskKeyFileInfo (..))
import Hyperion.Scheduler.Types    (MemorySize, NumCPUs, defaultRuntimeEstimate)

-- | We allow minThreads and maxThreads to depend on the stage of the
-- computation (and, at the 'Hyperion.Scheduler.Task.Task.TaskKey' level, on
-- the task config). TODO: Really, minThreads and maxThreads should be able
-- to depend on stats. How do we achieve that?
data RunStage = InitialRun | InProgressRun

type Tag = Text

-- Everything Scheduler needs
-- HasTaskHash + IsTask + CanRemoteRunTask + 'taskStatKey'
class (Typeable a, ToJSON a, Eq a, Ord a) => IsTask a where
--  taskHash :: a -> ByteString
--  default taskHash :: Binary a => a -> ByteString
--  taskHash = hashBase64SafeByteString

  -- | Estimated memory in bytes
  taskMemoryEstimate     :: a -> MemorySize
  taskMemoryEstimate = const 0
  -- | Estimated runtime in seconds, as a function of NumCPUs
  taskRuntimeEstimate    :: a -> NumCPUs -> NominalDiffTime
  taskRuntimeEstimate t = defaultRuntimeEstimate (taskMemoryEstimate t)
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

  -- | Whether this task is a placeholder (see 'Hyperion.Scheduler.Task.Task.TaskKind')
  -- that must be replaced before running.
  -- 'Hyperion.Scheduler.Task.TaskMap.validateTaskMap' rejects maps still containing placeholders.
  taskIsPlaceholder :: a -> Bool
  taskIsPlaceholder = const False

  -- | For a placeholder task with underlying key of type @k@, recover the
  -- key. This lets replacement machinery (e.g. blocks-3d's expandBlockTasks)
  -- find placeholders of a given key type in a TaskMap without knowing the
  -- resolver or config types hidden inside the task.
  taskPlaceholderKey :: Typeable k => a -> Maybe k
  taskPlaceholderKey _ = Nothing

  -- | The serialized stat key under which this task's resource usage is
  -- recorded and looked up. For a 'Hyperion.Scheduler.Task.Task.Task' this is
  -- built from the task's own stat key, which is also what
  -- 'taskMemoryEstimate' and 'taskRuntimeEstimate' are computed from.
  --
  -- 'Nothing' means the task has no identity in statistics: it is neither
  -- recorded nor looked up, and its estimates are zero. That is the right
  -- answer for tasks performing no computation (no-ops, placeholders), whose
  -- only measurable quantity would be scheduler bookkeeping latency.
  --
  -- Defaults to 'Nothing', matching the default 'StatKeyOf' of 'Void' at the
  -- 'Hyperion.Scheduler.Task.Task.TaskKey' level: no statistics unless a task
  -- says otherwise. Defaulting instead to the whole task encoded as its own
  -- stat key would put every task in a group of one, which no curve can be
  -- fitted to. 'Hyperion.Scheduler.Task.TaskMap.uninstrumentedTaskTags'
  -- reports tasks that compute but leave this at 'Nothing'.
  taskStatKey :: a -> Maybe StatKey
  taskStatKey _ = Nothing

-- NB: 'memoryToCpuTimeApprox' and 'defaultRuntimeEstimate' now live in
-- 'Hyperion.Scheduler.Types', so that 'Hyperion.Scheduler.StatKey' can use
-- them for the default 'Hyperion.Scheduler.StatKey.runtimeEstimate' without
-- importing this module.

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
