{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StaticPointers        #-}
{-# LANGUAGE TypeApplications      #-}

-- | Growing the task graph during a run.
--
-- A running task may add tasks to the run it belongs to, through its
-- 'SchedulerHandle' ("Hyperion.Scheduler.SchedulerHandle"). The new tasks join the graph like any other:
-- they get priorities, records, progress reporting and file bookkeeping, and
-- they may depend on tasks already in the run (finished or not) or on each
-- other. The task that adds them does not wait for them to run, so no
-- resource is held across the addition and the deadlock argument of the
-- scheduler is unchanged.
--
-- The scheduler processes the addition before it processes the completion
-- of the task that requested it, because 'addTasks' returns only after the
-- scheduler's reply. So a task that depends on the requesting task cannot
-- start before the added tasks exist: a build whose later stages depend on
-- the results of earlier ones (a search that decides after each round what
-- the next round is) is expressed as tasks that add the next stage before
-- they finish.
--
-- Node-local files: a late task may only read node-local files that still
-- exist. A file is deleted once every task known to use it has finished, so
-- a producer whose outputs will be read by tasks added later must declare
-- 'Hyperion.Scheduler.Task.IsTask.taskKeepOutputs'; the scheduler refuses an
-- addition that asks for a file it has already deleted.
module Hyperion.Scheduler.Dynamic
  ( addFollowUp
  , addTasks
  , addTasksWith
  ) where

import Control.Distributed.Process     (Process, newChan, receiveChan,
                                        sendChan)
import Data.Binary                     (Binary)
import Data.Binary                     qualified as Binary
import Data.Map.Strict                 (Map)
import Data.Set                        (Set)
import Data.Typeable                   (Typeable)
import Hyperion                        (Closure, Dict (..), Static (..), cAp,
                                        cPure)
import Hyperion.Log                    qualified as Log
import Bootstrap.Build                 (Variant)
import Hyperion.Scheduler.SchedulerHandle (AddTasksPayload (..),
                                           AddTasksReply (..),
                                           AddTasksRequest (..),
                                           SchedulerHandle (..),
                                           TaskHandle (..))
import Hyperion.Scheduler.Task.FollowUps (BinaryVariant, encodeFollowUp)
import Hyperion.Scheduler.Task.IsTask  (IsTask)
import Hyperion.Scheduler.Task.Task    (FollowUps)

-- | A task map with the 'Static' dictionary its 'Binary' instance needs to
-- travel inside a closure.
newtype StaticTaskMap a = MkStaticTaskMap (Map a (Set a))
  deriving newtype (Binary)

instance (Typeable a, Static (IsTask a), Static (Binary a)) => Static (Binary (StaticTaskMap a)) where
  closureDict = static (\Dict -> Dict) `cAp` closureDict @(IsTask a, Binary a)

-- | Top-level so that it can be referenced with @static@.
pureStaticTaskMap :: StaticTaskMap a -> Process (Map a (Set a))
pureStaticTaskMap (MkStaticTaskMap taskMap) = pure taskMap

-- | Add tasks to the run the handle belongs to, and return once the scheduler
-- has accepted them. Call this from inside a task, in the 'Process' monad on
-- a worker, before the task returns. Throws if the scheduler refuses: a
-- dependency that is not a task of the run, a dependency added to a task
-- that has already started, or a node-local input that has already been
-- deleted.
--
-- The task type must be the task type of the run (for a run built with
-- 'Hyperion.Scheduler.Task.TaskLink.mkTaskMap' that is
-- 'Hyperion.Scheduler.Task.WrappedTask.WrappedTask', which has no 'Binary'
-- instance; use 'addTasksWith' there).
addTasks
  :: forall a
   . (Static (IsTask a), Static (Binary a))
  => SchedulerHandle
  -> Map a (Set a)
  -> Process ()
addTasks handle taskMap =
  addTasksWith handle $
    static pureStaticTaskMap `cAp` cPure (MkStaticTaskMap taskMap)

-- | Like 'addTasks', but the tasks are built on the scheduler's side by the
-- given closure, in the 'Process' monad of the scheduler's node. Use this when
-- the task type cannot be sent over the wire, or when building the tasks
-- needs a check on the scheduler's side (for example, skipping tasks whose
-- outputs already exist).
addTasksWith :: SchedulerHandle -> Closure (Process (Map a (Set a))) -> Process ()
addTasksWith handle buildTasks = request handle (AddTasksClosure (Binary.encode buildTasks))

-- | Add one of the follow-ups the task's key declares
-- ('Hyperion.Scheduler.Task.Task.FollowUps') to the run, and return once the
-- scheduler has accepted the tasks. The task names the key; the scheduler
-- builds the key's task map with 'Hyperion.Scheduler.Task.TaskMap.mkTaskMap'
-- on its own side, using the resolver and configs the requesting task was
-- built with, and prunes the tasks whose outputs already exist there. The
-- requesting task therefore needs no resolver of its own. Throws if the
-- scheduler refuses, as 'addTasks' does.
addFollowUp
  :: forall k . BinaryVariant (FollowUps k)
  => TaskHandle k -> Variant (FollowUps k) -> Process ()
addFollowUp (MkTaskHandle handle) followUp = request handle (AddFollowUp (encodeFollowUp followUp))

request :: SchedulerHandle -> AddTasksPayload -> Process ()
request handle payload = do
  (replySendPort, replyRecvPort) <- newChan
  Log.info "Requesting to add tasks (handle)" handle.handleId
  sendChan handle.requestPort MkAddTasksRequest
    { handleId  = handle.handleId
    , payload   = payload
    , replyPort = replySendPort
    }
  reply <- receiveChan replyRecvPort
  case reply of
    AddTasksDone        -> Log.info "Tasks added (handle)" handle.handleId
    AddTasksRefused msg -> Log.throwError $
      "Adding tasks on handle " <> show handle.handleId <> " was refused: " <> msg
