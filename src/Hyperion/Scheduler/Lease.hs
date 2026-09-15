{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE StaticPointers        #-}

-- | Leases: a running task's handle to the scheduler that runs it.
--
-- When the scheduler starts a task it hands it a 'Lease': the number of CPUs
-- reserved for the task and a channel back to the scheduler instance, valid
-- until the task returns. Through it a task can ask the scheduler to add
-- tasks to the run (see "Hyperion.Scheduler.Dynamic" for the request and
-- "Hyperion.Scheduler.RunTasks" for the handler). A task that never uses its
-- lease behaves exactly as before.
--
-- This module deliberately knows nothing about tasks: the request payload is
-- an opaque 'ByteString' so that "Hyperion.Scheduler.Task.IsTask" can depend
-- on it without a module cycle.
module Hyperion.Scheduler.Lease
  ( LeaseId (..)
  , Lease (..)
  , LeaseRequest (..)
  , LeaseReply (..)
  ) where

import Control.DeepSeq             (NFData)
import Control.Distributed.Process (SendPort)
import Data.Aeson                  (ToJSON)
import Data.Binary                 (Binary)
import Data.ByteString.Lazy        (ByteString)
import GHC.Generics                (Generic)
import Hyperion                    (Dict (..), Static (..))
import Hyperion.Scheduler.Types    (NumCPUs)

-- | Identifies one reservation within one top-level scheduler run.
newtype LeaseId = MkLeaseId Int
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Binary, ToJSON, NFData)

-- | What a running task holds.
data Lease = MkLease
  { leaseId     :: LeaseId
    -- | Number of CPUs reserved for the task.
  , numCpus     :: NumCPUs
    -- | Where to send requests for the scheduler that owns the lease.
  , requestPort :: SendPort LeaseRequest
  } deriving (Show, Generic, Binary)

instance Static (Binary Lease) where
  closureDict = static Dict

-- | A request from a running task to the scheduler that owns its lease: the
-- tasks to add, in the encoding described in "Hyperion.Scheduler.Dynamic".
data LeaseRequest = MkLeaseRequest
  { leaseId   :: LeaseId
  , payload   :: ByteString
  , replyPort :: SendPort LeaseReply
  } deriving (Show, Generic, Binary)

data LeaseReply
  = LeaseReplyDone ByteString
  | LeaseReplyError String
  deriving (Show, Generic, Binary)
