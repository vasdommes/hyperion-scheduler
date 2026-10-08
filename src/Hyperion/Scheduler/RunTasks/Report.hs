{-# LANGUAGE OverloadedStrings #-}

-- | What 'Hyperion.Scheduler.RunTasks.runTasks' logs about a task map's
-- estimates.
module Hyperion.Scheduler.RunTasks.Report
  ( reportBeforeRun
  ) where

import Control.Monad                                (forM_)
import Control.Monad.IO.Class                       (MonadIO)
import Data.Map.Strict                              qualified as Map
import Data.Set                                     qualified as Set
import Hyperion.Log                                 qualified as Log
import Hyperion.Scheduler.Config                    (Config)
import Hyperion.Scheduler.RunTasks.TaskDistribution (describeUnschedulable,
                                                     unschedulableTaskTags)
import Hyperion.Scheduler.Task                      (EstimatedTask, IsTask,
                                                     TaskMap,
                                                     describeInstrumentationGap,
                                                     taskInstrumentationGaps,
                                                     underestimatedMemoryTags)
import Hyperion.Scheduler.Types                     (Node)

-- | Warn about tasks scheduled on a guess, once per task type.
reportBeforeRun
  :: (IsTask a, MonadIO m)
  => Config -> [Node] -> TaskMap (EstimatedTask a) -> m ()
reportBeforeRun config nodes taskMap = do
  -- Not fatal, but a forgotten 'toStatKey' should be visible before the run.
  forM_ (Map.toList (taskInstrumentationGaps taskMap)) $ \(gap, tags) ->
    Log.warn ("Tasks " <> describeInstrumentationGap gap) (Set.toList tags)
  case Set.toList (underestimatedMemoryTags 2 taskMap) of
    []   -> pure ()
    tags -> Log.warn
      "Tasks used over twice the memory their own model predicts, so they may \
      \be killed for running out of memory where no statistics exist" tags
  let unschedulable = unschedulableTaskTags config nodes (Map.keys taskMap)
  forM_ (Map.toList unschedulable) $ \(reason, tags) ->
    Log.warn ("Tasks " <> describeUnschedulable reason) (Set.toList tags)
