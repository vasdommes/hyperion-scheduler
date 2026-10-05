{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Hyperion.Scheduler.Task.IsTask where

import Data.Aeson                  (ToJSON)
import Data.Maybe                  (isJust)
import Data.Set                    (Set)
import Data.Set                    qualified as Set
import Data.Text                   (Text)
import Data.Text                   qualified as Text
import Data.Time.Clock             (NominalDiffTime)
import Data.Typeable               (Proxy (..), Typeable, typeRep)
import Debug.Trace                 qualified as Debug
import Hyperion                    (Closure, Process)
import Hyperion.Scheduler.FilePath (VirtualFilePath)
import Hyperion.Scheduler.StatKey  (TaskKeyFileInfo (..))
import Hyperion.Scheduler.Types    (MemorySize, NumCPUs)

-- | We allow minThreads and maxThreads to depend on the stage of the
-- computation (and, at the 'Hyperion.Scheduler.Task.Task.TaskKey' level, on
-- the task config). TODO: Really, minThreads and maxThreads should be able
-- to depend on stats. How do we achieve that?
data RunStage = InitialRun | InProgressRun
  deriving (Eq, Ord, Show)

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

  taskTag :: a -> Maybe Tag
  taskTag = defaultTaskTag

  taskClosure :: a -> Maybe (NumCPUs -> Closure (Process ()))

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

-- TODO: remove (Stats.ToStatKey a) and use (IsTask a) everywhere in Stats instead?
--  taskStatKey :: a -> StatKey
--  -- | A default implementation for the case where we wish to retain
--  -- all the information about a task in the StatKey.
--  taskStatKey = mkStatKeyViaJSON

-- | Default task tag = its type.
defaultTaskTagForType :: forall a. Typeable a => Maybe Tag
defaultTaskTagForType = Just $ Text.pack $ show $ typeRep $ Proxy @a

defaultTaskTag :: forall a. Typeable a => a -> Maybe Tag
defaultTaskTag _ = defaultTaskTagForType @a

-- If you don't have a better way to estimate runtime of your task, try this one.
-- It produces reasonably-looking times.
-- The constant 1.7e-6 originally came from our blocks_3d tests on Expanse.
memoryToCpuTimeApprox :: MemorySize -> NominalDiffTime
memoryToCpuTimeApprox mem = 1.7e-6 * fromIntegral mem

-- | Estimate runtime from memory when no task-specific estimate is available.
-- Zero CPUs is valid for scheduler-only tasks which do not perform computation.
defaultRuntimeEstimate :: MemorySize -> NumCPUs -> NominalDiffTime
defaultRuntimeEstimate _   0       = 0
defaultRuntimeEstimate _   numCpus | numCpus < 0 = error "defaultRuntimeEstimate: negative CPU count"
defaultRuntimeEstimate mem numCpus = memoryToCpuTimeApprox mem / fromIntegral numCpus

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

taskHasClosure :: IsTask a => a -> Bool
taskHasClosure = isJust . taskClosure
