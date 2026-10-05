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
import Data.Map.Strict                qualified as Map
import Data.Set                       qualified as Set
import Hyperion.Scheduler.StatKey     (TaskKeyFileInfo (..))
import Hyperion.Scheduler.Stats       (TaskAndFileStats, Trials (..),
                                       approxRuntime, fileSizeCorrection,
                                       lookupFileStats, lookupTaskStats,
                                       maxMemory, memoryCorrection,
                                       runtimeCorrection)
import Hyperion.Scheduler.Task.IsTask (IsTask (..), ResourceEstimates (..),
                                       TaskSummary (..))
import Hyperion.Scheduler.Types       (correctWithStats, modelEstimate,
                                       overrideWithMeasured)

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
-- For each figure, best first:
--
-- 1. Statistics recorded for the same stat key and input summary: the
--    measured figure ('MeasuredFromStats').
-- 2. Statistics recorded for the same stat key and close input summaries
--    ('closeToInputSummary'): the task's own model, corrected by how far the
--    measurements were from the model's predictions for their inputs
--    ('CorrectedByStats'). See 'memoryCorrection', 'runtimeCorrection' and
--    'fileSizeCorrection'.
-- 3. Otherwise the task's own model.
--
-- A miss shows in the resulting 'Estimate's. Memory and runtime are replaced
-- independently: memory statistics are absent whenever no run recorded a
-- memory figure, while runtime statistics are always recorded.
decorateSummaryWithStats :: TaskAndFileStats -> TaskSummary -> TaskSummary
decorateSummaryWithStats stats summary = summary
  { outputs   = Set.map decorateFile summary.outputs
  , estimates = MkResourceEstimates { memory = memory, runtime = runtime }
  }
  where
    own = summary.estimates
    recorded = maybe Map.empty (`lookupTaskStats` stats) summary.statKey
    exact = summary.inputSummary >>= (`Map.lookup` recorded)
    -- Each with the model's estimates for its inputs.
    close =
      [ (modelFor, resources)
      | (s, resources) <- Map.toList recorded
      , Just s /= summary.inputSummary
      , summary.closeToInputSummary s
      , Just modelFor <- [summary.model s]
      ]
    memory
      | Just measured <- exact >>= maxMemory = overrideWithMeasured measured own.memory
      | Just factor <- memoryCorrection
          [ (modelEstimate modelFor.memory, resources) | (modelFor, resources) <- close ] =
          correctWithStats factor (scale factor (modelEstimate own.memory)) own.memory
      | otherwise = own.memory
    runtime
      | Just measured <- exact >>= approxRuntime Nothing = overrideWithMeasured measured own.runtime
      | Just (factor, corrected) <- runtimeCorrection (modelEstimate own.runtime)
          [ (modelEstimate modelFor.runtime, resources) | (modelFor, resources) <- close ] =
          correctWithStats factor corrected own.runtime
      | otherwise = own.runtime

    -- A file with no stat key is never looked up and keeps its estimate.
    decorateFile info = info { fileSize = fileSize } where
      recordedSizes = maybe Map.empty (`lookupFileStats` stats) info.fileStatKey
      fileSize
        | Just trials <- summary.producerSummary >>= (`Map.lookup` recordedSizes) =
            overrideWithMeasured trials.max info.fileSize
        | Just factor <- fileSizeCorrection
            [ (modelSize, trials)
            | (s, trials) <- Map.toList recordedSizes
            , Just s /= summary.producerSummary
            , summary.closeToProducerSummary info.path s
            , Just modelSize <- [Map.lookup info.path =<< summary.outputModel s]
            ] =
            correctWithStats factor (scale factor (modelEstimate info.fileSize)) info.fileSize
        | otherwise = info.fileSize

    scale :: Integral a => Double -> a -> a
    scale factor x = ceiling (factor * fromIntegral x)

-- | 'decorateSummaryWithStats' for a wrapped task.
decorateTaskWithStats :: TaskAndFileStats -> WrappedTask -> WrappedTask
decorateTaskWithStats stats task = task { summary = decorateSummaryWithStats stats task.summary }
