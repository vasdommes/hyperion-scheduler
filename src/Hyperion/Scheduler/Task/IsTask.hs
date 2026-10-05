{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Hyperion.Scheduler.Task.IsTask where

import Data.Aeson                  (ToJSON)
import Data.Map.Strict             (Map)
import Data.Maybe                  (fromMaybe, isJust)
import Data.Set                    (Set)
import Data.Set                    qualified as Set
import Data.Text                   (Text)
import Data.Time.Clock             (NominalDiffTime)
import Data.Typeable               (Typeable)
import Debug.Trace                 qualified as Debug
import Hyperion                    (Closure, Process)
import Hyperion.Scheduler.FilePath (VirtualFilePath)
import Hyperion.Scheduler.StatKey  (EncodedSummary, StatKey,
                                    TaskKeyFileInfo (..), unitSummary)
import Hyperion.Scheduler.Types    (Estimate (..), FileSize, MemorySize,
                                    NumCPUs, defaultRuntimeEstimate,
                                    schedulingEstimate)
import Hyperion.Scheduler.Util     (typeRepText)

-- | We allow minThreads and maxThreads to depend on the stage of the
-- computation (and, at the 'Hyperion.Scheduler.Task.Task.TaskKey' level, on
-- the task config). TODO: Really, minThreads and maxThreads should be able
-- to depend on stats. How do we achieve that?
data RunStage = InitialRun | InProgressRun
  deriving (Eq, Ord, Show)

type Tag = Text

-- Everything Scheduler needs
-- HasTaskHash + IsTask + CanRemoteRunTask + 'taskSummary'
class (Typeable a, ToJSON a, Eq a, Ord a) => IsTask a where
--  taskHash :: a -> ByteString
--  default taskHash :: Binary a => a -> ByteString
--  taskHash = hashBase64SafeByteString

  -- | The task's files, stat key and estimates, computed together because
  -- they share expensive work.
  --
  -- @Just knownInputs@ gives the infos (and so the sizes) of the input files
  -- that are known: those produced by other tasks in the map, and those on
  -- disk (see 'Hyperion.Scheduler.Task.EstimatedTaskMap.mkEstimatedTaskMap').
  -- An input missing from them keeps its own info. 'Nothing' gives the task's
  -- summary as it is, e.g. the one a
  -- 'Hyperion.Scheduler.Task.WrappedTask.WrappedTask' holds.
  taskSummary :: Maybe KnownInputs -> a -> TaskSummary

  -- | Maximum possible threads for the task
  -- TODO: get rid of RunStage?
  taskMaxThreads :: RunStage -> a -> NumCPUs
  taskMaxThreads _ _ = 1
  -- | Minimum possible threads for the task
  taskMinThreads :: RunStage -> a -> NumCPUs
  taskMinThreads _ _ = 1
  taskDefaultPriority   :: a -> Int
  taskDefaultPriority = const 0

  -- | A label indicating the type of task. If Nothing, the task
  -- will be ommitted from progress reports.
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

-- | Infos of input files, by path.
type KnownInputs = VirtualFilePath -> Maybe TaskKeyFileInfo

-- | See 'taskSummary'.
data TaskSummary = MkTaskSummary
  { inputs      :: Set TaskKeyFileInfo
  , outputs     :: Set TaskKeyFileInfo
    -- | The serialized stat key under which this task's resource usage is
    -- recorded and looked up. For a 'Hyperion.Scheduler.Task.Task.Task' this
    -- is built from the task's own stat key, which is also what 'estimates'
    -- are computed from.
    --
    -- 'Nothing' means the task has no identity in statistics: it is neither
    -- recorded nor looked up, and its estimates are zero. That is the right
    -- answer for tasks performing no computation (no-ops, placeholders),
    -- whose only measurable quantity would be scheduler bookkeeping latency.
    -- Setting it instead to the whole task encoded as its own stat key would
    -- put every task in a group of one, which no curve can be fitted to.
  , statKey     :: Maybe StatKey
    -- | Estimated memory in bytes and runtime in seconds, with the provenance
    -- of each. Ordinary tasks give their own model (e.g.
    -- 'estimatesFromModel'); only
    -- 'Hyperion.Scheduler.Task.WrappedTask.decorateTaskWithStats' ever
    -- reports a figure measured from statistics.
  , estimates   :: ResourceEstimates
    -- | The input summary 'estimates' are computed from, encoded; 'Nothing' if
    -- the task has no stat key.
  , inputSummary :: Maybe EncodedSummary
    -- | Whether statistics recorded with the given input summary can correct
    -- 'estimates' (see 'Hyperion.Scheduler.StatKey.closeInputSummaries').
  , closeToInputSummary :: EncodedSummary -> Bool
    -- | The task's own model for the given input summary, 'Nothing' if it
    -- does not decode (e.g. after the summary type changed) or the task has
    -- no stat key. It lets statistics recorded with other inputs be compared
    -- with the model without the typed stat key.
  , model       :: EncodedSummary -> Maybe ResourceEstimates
    -- | The producer input summary the sizes of the output files are computed
    -- from, encoded; 'Nothing' if they have no file stat key.
  , producerSummary :: Maybe EncodedSummary
    -- | 'closeToInputSummary' for the size of an output file.
  , closeToProducerSummary :: VirtualFilePath -> EncodedSummary -> Bool
    -- | 'model' for the sizes of the output files, given the producer input
    -- summary.
  , outputModel :: EncodedSummary -> Maybe (Map VirtualFilePath FileSize)
  }

-- | The summary of a task with no stat key: its own files and zero
-- estimates. The sizes of its output files ignore its inputs (summary '()').
filesOnlySummary
  :: Set TaskKeyFileInfo  -- ^ its own inputs
  -> Set TaskKeyFileInfo  -- ^ outputs
  -> Maybe KnownInputs
  -> TaskSummary
filesOnlySummary ownInputs outputs knownInputs = MkTaskSummary
  { inputs      = maybe ownInputs withKnown knownInputs
  , outputs     = outputs
  , statKey     = Nothing
  , estimates   = estimatesFromModel 0
  , inputSummary = Nothing
  , closeToInputSummary = const False
  , model       = const Nothing
  , producerSummary = Just unitSummary
  , closeToProducerSummary = \_ _ -> False
  , outputModel = const Nothing
  }
  where
    withKnown known = Set.map (\i -> fromMaybe i (known i.path)) ownInputs

-- | Input files, with their sizes.
taskInputs :: IsTask a => a -> Set TaskKeyFileInfo
taskInputs = (.inputs) . taskSummary Nothing

-- | Output files, with their size estimates.
taskOutputs :: IsTask a => a -> Set TaskKeyFileInfo
taskOutputs = (.outputs) . taskSummary Nothing

-- | See 'TaskSummary'.
taskStatKey :: IsTask a => a -> Maybe StatKey
taskStatKey = (.statKey) . taskSummary Nothing

-- | See 'TaskSummary'.
taskResourceEstimates :: IsTask a => a -> ResourceEstimates
taskResourceEstimates = (.estimates) . taskSummary Nothing

-- | A task's memory and runtime estimates with their provenance.
data ResourceEstimates = MkResourceEstimates
  { memory  :: Estimate MemorySize
  , runtime :: Estimate (NumCPUs -> NominalDiffTime)
  }

-- | Default task tag = its type.
defaultTaskTagForType :: forall a. Typeable a => Maybe Tag
defaultTaskTagForType = Just $ typeRepText @a

defaultTaskTag :: forall a. Typeable a => a -> Maybe Tag
defaultTaskTag _ = defaultTaskTagForType @a

-- | A task's own model: the memory it predicts, and a runtime curve derived
-- from that memory. A task with a runtime model of its own builds
-- 'MkResourceEstimates' directly instead.
estimatesFromModel :: MemorySize -> ResourceEstimates
estimatesFromModel memory = MkResourceEstimates
  { memory  = EstimatedByTask memory
  , runtime = EstimatedByTask (defaultRuntimeEstimate memory)
  }

-- | Estimated memory in bytes: the figure the task is scheduled on, whether
-- that is the task's own prediction or one measured from statistics.
taskMemoryEstimate :: IsTask a => a -> MemorySize
taskMemoryEstimate t = schedulingEstimate (taskResourceEstimates t).memory

-- | Estimated runtime in seconds, as a function of NumCPUs. See
-- 'taskMemoryEstimate'.
taskRuntimeEstimate :: IsTask a => a -> NumCPUs -> NominalDiffTime
taskRuntimeEstimate t = schedulingEstimate (taskResourceEstimates t).runtime

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

taskHasClosure :: IsTask a => a -> Bool
taskHasClosure = isJust . taskClosure
