{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE StaticPointers        #-}

-- | A running task's handle to the scheduler that runs it.
--
-- When the scheduler starts a task that asked for one (a
-- 'Hyperion.Scheduler.Task.Task.CustomTaskWithHandle', or an
-- 'Hyperion.Scheduler.Task.IsTask.taskClosureWithHandle'), it hands the task
-- a 'SchedulerHandle': a channel back to the scheduler instance, valid until
-- the task returns. Through it a task can ask the scheduler to add tasks to
-- the run (see "Hyperion.Scheduler.Dynamic" for the request and
-- "Hyperion.Scheduler.RunTasks" for the handler). The handle's id lets the
-- scheduler refuse requests from tasks that have already returned.
--
-- This module deliberately knows nothing about tasks: the request payload is
-- an opaque 'ByteString' so that "Hyperion.Scheduler.Task.IsTask" can depend
-- on it without a module cycle.
module Hyperion.Scheduler.SchedulerHandle
  ( HandleId (..)
  , SchedulerHandle (..)
  , TaskHandle (..)
  , AddTasksRequest (..)
  , AddTasksPayload (..)
  , AddTasksReply (..)
  ) where

import Control.DeepSeq             (NFData)
import Control.Distributed.Process (SendPort)
import Data.Aeson                  (ToJSON)
import Data.Binary                 (Binary)
import Data.ByteString.Lazy        (ByteString)
import Data.Kind                   (Type)
import GHC.Generics                (Generic)
import Hyperion                    (Dict (..), Static (..))

-- | Identifies one running task within one scheduler run.
newtype HandleId = MkHandleId Int
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Binary, ToJSON, NFData)

-- | What a running task holds.
data SchedulerHandle = MkSchedulerHandle
  { handleId    :: HandleId
    -- | Where to send requests for the scheduler instance running the task.
  , requestPort :: SendPort AddTasksRequest
  } deriving (Show, Generic, Binary)

instance Static (Binary SchedulerHandle) where
  closureDict = static Dict

-- | A 'SchedulerHandle' as a 'Hyperion.Scheduler.Task.Task.TaskKey' task
-- body receives it: tagged with the task's key type, so that the follow-ups
-- the body asks for ('Hyperion.Scheduler.Dynamic.addFollowUp') are checked
-- against the key's declaration ('Hyperion.Scheduler.Task.Task.FollowUps').
-- Made on the worker from the plain handle; never sent.
newtype TaskHandle (k :: Type) = MkTaskHandle { schedulerHandle :: SchedulerHandle }
  deriving newtype (Show)

-- | A request from a running task to its scheduler: tasks to add, either as
-- a closure that builds them (the encoding described in
-- "Hyperion.Scheduler.Dynamic") or as one of the follow-ups the task's key
-- declares (an encoded 'Bootstrap.Build.Variant' of the declared key types,
-- see "Hyperion.Scheduler.Task.FollowUps").
data AddTasksRequest = MkAddTasksRequest
  { handleId  :: HandleId
  , payload   :: AddTasksPayload
  , replyPort :: SendPort AddTasksReply
  } deriving (Show, Generic, Binary)

data AddTasksPayload
  = AddTasksClosure ByteString
  | AddFollowUp ByteString
  deriving (Show, Generic, Binary)

data AddTasksReply
  = AddTasksDone
  | AddTasksRefused String
  deriving (Show, Generic, Binary)
