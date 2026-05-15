{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StaticPointers             #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Hyperion.Scheduler.RemoteUtil where


import Control.Concurrent                  (myThreadId)
import Control.Distributed.Process         (Closure, NodeId, Process, ProcessId,
                                            SendPort, newChan, receiveChan,
                                            spawnLocal)
import Control.Distributed.Process.Async   (Async, AsyncResult (..), async,
                                            asyncLinked, asyncWorker, task,
                                            wait)
import Control.Distributed.Process.Closure (SerializableDict (..))
import Control.Exception                   (Exception, throwTo)
import Control.Monad.IO.Class              (liftIO)
import Control.Monad.Reader                (ask, asks, runReaderT)
import Control.Monad.Trans                 (lift)
import Data.Binary                         (Binary)
import Data.Kind                           (Type)
import Data.Map.Strict                     qualified as Map
import Data.Typeable                       (Typeable)
import Hyperion                            (Dict (..), Job, JobEnv (..),
                                            Serializable, Static,
                                            WorkerAddr (..), cAp, closureDict,
                                            remoteEvalOnWorker)
import Hyperion                            qualified as Hyp
import Hyperion.CallClosure                (call')
import Hyperion.Scheduler.Config           (Config (..))
import Hyperion.Scheduler.Types            (Node (..))

-- TODO orphan instance
instance Static (Binary (SendPort ProcessId)) where
  closureDict = static Dict

-- | Get all the nodes accessible to a Job
getJobNodes :: Config -> Job [Node]
getJobNodes config = do
  addrs <- asks (Map.keys . jobWorkerLauncherMap)
  Hyp.NumCPUs cpusPerNode <- asks jobNodeCpus
  pure $ do
    addr <- addrs
    pure $ MkNode
      { memory           = config.nodeMemory
      , cpus             = cpusPerNode
      , localStoragePath = config.localStoragePath
      , localStorageSize = config.nodeLocalStorageSize
      , address          = addr
      }


-- | Run a Job computation in a separate thread.
spawnLocalJob :: Job () -> Job ProcessId
spawnLocalJob j = do
  jobEnv <- ask
  lift $ spawnLocal $ runReaderT j jobEnv

-- | (async . task) but for Job monad
asyncLocalJob :: (Binary a, Typeable a) => Job a -> Job (Async a)
asyncLocalJob j = do
  jobEnv <- ask
  lift $ async $ task $ runReaderT j jobEnv

-- | (asyncLinked . task) but for Job monad
-- NB: here "linked" means that the task links to the parent, and will abort if it dies.
-- Parent does not link to the task!
-- Use e.g. throwOnAsyncFailed (see below) to monitor job failures.
asyncLinkedLocalJob :: (Binary a, Typeable a) => Job a -> Job (Async a)
asyncLinkedLocalJob j = do
  jobEnv <- ask
  lift $ asyncLinked $ task $ runReaderT j jobEnv

data AsyncFailedException= AsyncFailedException !ProcessId !String
  deriving (Show, Exception)

throwOnAsyncFailed :: (Show a) => Async a -> Process ()
throwOnAsyncFailed asyncHandle = do
  tid <- liftIO myThreadId
  let asyncPid = asyncWorker asyncHandle
  _ <- spawnLocal $ do
    result <- wait asyncHandle
    case result of
      AsyncDone _    -> return ()
      AsyncCancelled -> return ()
      _              -> liftIO $ throwTo tid $ AsyncFailedException asyncPid $ show result
  return ()

toSerializableDict :: Typeable a => Closure (Dict (Serializable a) -> SerializableDict a)
toSerializableDict = static (\Dict -> SerializableDict)

closureSerializableDict :: (Typeable a, Static (Binary a)) => Closure (SerializableDict a)
closureSerializableDict = toSerializableDict `cAp` closureDict

callOnNode :: (Typeable a, Static (Binary a)) => NodeId -> Closure (Process a) -> Process a
callOnNode = call' closureSerializableDict

class (
    Binary (RequestType service),
    Binary (ResponseType service),
    Binary (StartupResponseType service),
    Typeable (RequestType service),
    Typeable (ResponseType service),
    Typeable (StartupResponseType service),
    Show (ResponseType service)
  ) => Service (service :: Type) where
    type RequestType service :: Type
    type ResponseType service :: Type
    type StartupResponseType service :: Type
    getServiceProcessId :: StartupResponseType service -> ProcessId

-- | Starts service on a remote worker.
-- Accepts worker address and a function to build a Closure with service loop.
-- Returns SendPort for submitting requests to the service
startRemoteService :: forall service. Service service
                  => WorkerAddr
                  -> (SendPort (StartupResponseType service) -> Closure (Process ()))
                  -> Job ProcessId
startRemoteService address getServiceClosure = do
    (startupResponseSendPort, startupResponseReceivePort) <- lift newChan
    _ <- spawnLocalJob $
        remoteEvalOnWorker address $
        getServiceClosure startupResponseSendPort

    startupResponse <- lift $ receiveChan startupResponseReceivePort
    return $ getServiceProcessId @service startupResponse
