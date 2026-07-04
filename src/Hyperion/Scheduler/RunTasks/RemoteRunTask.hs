{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StaticPointers        #-}

module Hyperion.Scheduler.RunTasks.RemoteRunTask
  ( RemoteRunTaskResult(..)
  , emptyRemoteRunTaskResult
  , remoteRunTask
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader          (local)
import Data.Binary                   (Binary)
import Data.Map.Strict               (Map)
import Data.Map.Strict               qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics                  (Generic)
import Hyperion (Job, Static, cAp, cPure, setTaskCpus)
import Hyperion                      qualified as Hyp
import Hyperion.Log                  qualified as Log
import Hyperion.Scheduler.FilePath (VirtualFilePath (VirtualFilePath))
import Hyperion.Scheduler.IsTask (IsTask (..), taskOutputPaths)
import Hyperion.Scheduler.Types      (FileSize (..), MemorySize (..), NumCPUs)
import Hyperion.Scheduler.WorkerPool (TWorker, remoteRunOnNewWorker)
import System.Directory.OsPath (getFileSize)
import System.RUsage qualified as RUsage

data RemoteRunTaskResult = MkRemoteRunTaskResult
  { remoteTaskMemory    :: Maybe MemorySize
  , remoteTaskFileSizes :: Map VirtualFilePath FileSize
  }
  deriving (Generic, Binary, Show)

instance Static (Binary Hyperion.Scheduler.RunTasks.RemoteRunTask.RemoteRunTaskResult) where
  closureDict = static Hyp.Dict

emptyRemoteRunTaskResult :: RemoteRunTaskResult
emptyRemoteRunTaskResult = MkRemoteRunTaskResult { remoteTaskMemory = Nothing, remoteTaskFileSizes = Map.empty }

afterReturnMemoryM :: MonadIO m => m () -> m (Maybe MemorySize)
afterReturnMemoryM go = do
  go
  let kilobytesToBytes k = 1024 * fromIntegral k
  rSelf     <- liftIO $ RUsage.get RUsage.Self
  rChildren <- liftIO $ RUsage.get RUsage.Children
  pure $ Just $ MemorySize $ kilobytesToBytes $ max rSelf.maxResidentSetSize rChildren.maxResidentSetSize

afterReturnRemoteRunTaskResultM
  :: MonadIO m
  => Set VirtualFilePath
  -> m ()
  -> m RemoteRunTaskResult
afterReturnRemoteRunTaskResultM files go = do
  mem <- afterReturnMemoryM go
  let
    getFileSize' (VirtualFilePath p) = liftIO $ getFileSize p
    pathAndSize p = do
      size <- getFileSize' p
      return (p, fromIntegral size)
  fileSizes <- Map.fromList <$> (mapM pathAndSize $ Set.toList files)
  return $ MkRemoteRunTaskResult { remoteTaskMemory = mem, remoteTaskFileSizes = fileSizes }

remoteRunTask :: IsTask a => Maybe TWorker -> NumCPUs -> a -> Job RemoteRunTaskResult
remoteRunTask mWorker numCpus task = case taskClosure numCpus task of
  Nothing -> pure emptyRemoteRunTaskResult
  Just closure -> case mWorker of
    Nothing -> Log.throwError "remoteRunTask expected (Just TWorker), but got Nothing"
    Just w -> do
      -- TODO: is setTaskCpus really needed?
      local (setTaskCpus (Hyp.NumCPUs numCpus)) $
        remoteRunOnNewWorker w $
        static afterReturnRemoteRunTaskResultM
        -- TODO: measure input file sizes too?
        `cAp` cPure (taskOutputPaths task)
        `cAp` closure

