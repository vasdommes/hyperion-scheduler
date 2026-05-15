{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StaticPointers        #-}

module Hyperion.Scheduler.RunTasks.CanRemoteRunTask
  ( RemoteRunTaskResult(..)
  , CanRemoteRunTask (..)
  , emptyRemoteRunTaskResult
  , remoteRunOnNode
  )
where

import Control.Monad.Reader          (local)
import Data.Binary                   (Binary)
import Data.Map.Strict               (Map)
import Data.Map.Strict               qualified as Map
import GHC.Generics                  (Generic)
import Hyperion                      (Closure, Job, Process, Static,
                                      setTaskCpus)
import Hyperion                      qualified as Hyp
import Hyperion.Log                  qualified as Log
import Hyperion.Scheduler.FilePath   (VirtualFilePath)
import Hyperion.Scheduler.Types      (FileSize (..), MemorySize (..), NumCPUs)
import Hyperion.Scheduler.WorkerPool (TWorker, remoteRunOnNewWorker)
import Type.Reflection               (Typeable)

data RemoteRunTaskResult = MkRemoteRunTaskResult
  { remoteTaskMemory    :: Maybe MemorySize
  , remoteTaskFileSizes :: Map VirtualFilePath FileSize
  }
  deriving (Generic, Binary, Show)

instance Static (Binary Hyperion.Scheduler.RunTasks.CanRemoteRunTask.RemoteRunTaskResult) where
  closureDict = static Hyp.Dict

emptyRemoteRunTaskResult :: RemoteRunTaskResult
emptyRemoteRunTaskResult = MkRemoteRunTaskResult { remoteTaskMemory = Nothing, remoteTaskFileSizes = Map.empty }

-- | The return value should be the memory usage in Bytes, if it is
-- known.
class CanRemoteRunTask a where
  remoteRunTask :: (Maybe TWorker) -> NumCPUs -> a -> Job RemoteRunTaskResult

-- | Unit is sometimes useful as a top-level placeholder.
instance CanRemoteRunTask () where
  remoteRunTask _ _ _ = pure emptyRemoteRunTaskResult

-- | A helper function for defining instances of
-- CanRemoteRunTask. Sets numCpus and runs the given Closure on the given TWorker from a node.
remoteRunOnNode :: (Typeable a, Static (Binary a)) => (Maybe TWorker) -> NumCPUs -> Closure (Process a) -> Job a
remoteRunOnNode mWorker numCpus closure =
  case mWorker of
    Nothing -> Log.throwError "remoteRunOnNode expected (Just TWorker), but got Nothing"
    Just w -> do
      -- TODO: is setTaskCpus really needed?
      local (setTaskCpus (Hyp.NumCPUs numCpus)) $
        remoteRunOnNewWorker w closure
