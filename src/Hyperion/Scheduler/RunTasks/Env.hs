{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StaticPointers        #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | State shared by the scheduler instances of one run.
--
-- 'Hyperion.Scheduler.RunTasks.runTasksIn' runs one scheduling loop over a
-- 'ResourcePool'; the top-level run owns every node of the job. Instances
-- share one 'SchedulerEnv': the configuration, the node-local file
-- bookkeeping and the registry of handles held by running tasks.
module Hyperion.Scheduler.RunTasks.Env
  ( SchedulerEnv (..)
  , ResourcePool (..)
  , StaticTaskMap (..)
  , HandleRegistry
  , newHandleRegistry
  , registerHandle
  , unregisterHandle
  , isHandleActive
  ) where

import Data.Binary                    (Binary)
import Data.IORef                     (IORef, atomicModifyIORef', newIORef,
                                       readIORef)
import Data.Map.Strict                (Map)
import Data.Set                       (Set)
import Data.Set                       qualified as Set
import Data.Typeable                  (Typeable)
import Hyperion                       (Dict (..), Static (..), cAp)
import Hyperion.Scheduler.Config      (Config)
import Hyperion.Scheduler.FileService (FileService)
import Hyperion.Scheduler.SchedulerHandle (HandleId)
import Hyperion.Scheduler.Task.IsTask (IsTask)
import Hyperion.Scheduler.Types       (Node)
import Hyperion.Scheduler.WorkerPool  (WorkerPool)

-- | Services and state shared by every scheduler instance of one run.
data SchedulerEnv = MkSchedulerEnv
  { config       :: Config
  , fileService  :: FileService
    -- | Handles currently held by running tasks.
  , handles       :: HandleRegistry
    -- | Source of handle ids, shared by every instance so that ids are unique
    -- across the whole run.
  , handleCounter :: IORef Int
  }

-- | The resources one scheduler instance may use: the capacities it may fill
-- (one 'Node' per address; a 'Node' may describe a slice of a physical node)
-- and the worker slots it runs tasks on.
data ResourcePool = MkResourcePool
  { nodes      :: [Node]
  , workerPool :: WorkerPool
  }

-- | The handles the scheduler has handed out and not taken back: a request
-- on any other id is refused.
newtype HandleRegistry = MkHandleRegistry (IORef (Set HandleId))

newHandleRegistry :: IO HandleRegistry
newHandleRegistry = MkHandleRegistry <$> newIORef Set.empty

registerHandle :: HandleRegistry -> HandleId -> IO ()
registerHandle (MkHandleRegistry ref) handleId =
  atomicModifyIORef' ref $ \s -> (Set.insert handleId s, ())

unregisterHandle :: HandleRegistry -> HandleId -> IO ()
unregisterHandle (MkHandleRegistry ref) handleId =
  atomicModifyIORef' ref $ \s -> (Set.delete handleId s, ())

isHandleActive :: HandleRegistry -> HandleId -> IO Bool
isHandleActive (MkHandleRegistry ref) handleId = Set.member handleId <$> readIORef ref

-- | A task map together with the 'Static' dictionaries needed to put it in a
-- closure, so that it can travel from a task to the scheduler (see
-- "Hyperion.Scheduler.Dynamic").
newtype StaticTaskMap a = MkStaticTaskMap (Map a (Set a))
  deriving newtype (Binary)

instance (Typeable a, Static (IsTask a), Static (Binary a)) => Static (Binary (StaticTaskMap a)) where
  closureDict = static (\Dict -> Dict) `cAp` closureDict @(IsTask a, Binary a)
