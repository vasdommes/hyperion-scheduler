{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | What 'Hyperion.Scheduler.RunTasks.runTasks' logs about a task map's
-- estimates.
module Hyperion.Scheduler.RunTasks.Report
  ( reportBeforeRun
  , reportAfterRun
  ) where

import Control.Monad                                (forM_, when)
import Control.Monad.IO.Class                       (MonadIO)
import Data.List                                    (sortOn)
import Data.Map.Strict                              qualified as Map
import Data.Maybe                                   (fromMaybe)
import Data.Set                                     qualified as Set
import Hyperion.Log                                 qualified as Log
import Hyperion.Scheduler.Config                    (Config)
import Hyperion.Scheduler.RunTasks.TaskDistribution (describeUnschedulable,
                                                     unschedulableTaskTags)
import Hyperion.Scheduler.Stats                     (Accuracy (..),
                                                     TaskRecord (..),
                                                     Trials (..), accuracyBy)
import Hyperion.Scheduler.Task                      (EstimatedTask, IsTask (..),
                                                     StatsCoverage (..),
                                                     TaskMap,
                                                     describeInstrumentationGap,
                                                     statsCoverage,
                                                     taskInstrumentationGaps,
                                                     underestimatedMemoryTags)
import Hyperion.Scheduler.Types                     (Node, modelEstimate)

-- | Warn about tasks scheduled on a guess, once per task type, and log how
-- many tasks are estimated from recorded statistics.
reportBeforeRun
  :: (IsTask a, MonadIO m)
  => Config -> [Node] -> TaskMap (EstimatedTask a) -> m ()
reportBeforeRun config nodes taskMap = do
  -- Not fatal, but a forgotten 'toStatKey' should be visible before the run.
  forM_ (Map.toList (taskInstrumentationGaps taskMap)) $ \(gap, tags) ->
    Log.warn ("Tasks " <> describeInstrumentationGap gap) (Set.toList tags)
  -- Zero coverage with statistics given usually means that the keys no longer
  -- match, e.g. after a stat key type was renamed.
  case statsCoverage taskMap of
    MkStatsCoverage { withStatKey = 0 } -> pure ()
    coverage -> do
      Log.info
        "Tasks estimated from recorded statistics (measured, corrected from \
        \close inputs, of those with a stat key)"
        (coverage.measured, coverage.corrected, coverage.withStatKey)
      -- An estimated map does not say whether any statistics were given, so
      -- this cannot tell a first run from a run whose keys stopped matching.
      when (coverage.measured + coverage.corrected == 0) $ Log.warn
        "No task matched any recorded statistics, so every estimate is the \
        \task's own model. Expected on a first run; otherwise the stat keys no \
        \longer match these tasks' (tasks with a stat key)"
        coverage.withStatKey
  case Set.toList (underestimatedMemoryTags 2 taskMap) of
    []   -> pure ()
    tags -> Log.warn
      "Tasks used over twice the memory their own model predicts, so they may \
      \be killed for running out of memory where no statistics exist" tags
  let unschedulable = unschedulableTaskTags config nodes (Map.keys taskMap)
  forM_ (Map.toList unschedulable) $ \(reason, tags) ->
    Log.warn ("Tasks " <> describeUnschedulable reason) (Set.toList tags)

-- | Log how this run's measurements compare with the tasks' own models,
-- grouped by tag to keep it short. Unlike 'underestimatedMemoryTags' before the
-- run, this covers tasks with no statistics.
reportAfterRun :: (IsTask a, MonadIO m) => [TaskRecord a] -> m ()
reportAfterRun records
  | null worst = pure ()
  | otherwise  = do
      Log.info
        "Measured over predicted by the tasks' own models, worst first \
        \(tag, memory worst, memory mean, runtime mean, observations)"
        (map summarise worst)
      case [tag | (tag, accuracy) <- worst, worstMemory accuracy > 2] of
        []   -> pure ()
        tags -> Log.warn
          "Some task used over twice the memory its own model predicts in this \
          \run, so it would be under-allocated on a machine with no statistics \
          \to correct the model" tags
  where
    -- The worst observation, not the mean: one task far above its model is the
    -- one that runs out of memory.
    worst = sortOn (negate . worstMemory . snd) rows
    worstMemory accuracy = maybe 0 (.max) accuracy.memory
    -- Memory and runtime are scored independently; a group appears if either
    -- is.
    rows =
      [ (fromMaybe "untagged" tag, accuracy)
      | (tag, accuracy) <- Map.toList byTag
      , observations accuracy > 0
      ]
    summarise (tag, accuracy) =
      ( tag
      , (.max) <$> accuracy.memory
      , (.mean) <$> accuracy.memory
      , (.mean) <$> accuracy.runtime
      , observations accuracy
      )
    observations accuracy =
      max (trials accuracy.memory) (trials accuracy.runtime)
    trials = maybe 0 (.numTrials)
    byTag = accuracyBy (Just . taskTag . (.task)) modelEstimate records
