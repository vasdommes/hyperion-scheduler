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
  , parseVmHWM
  , peakResidentSetSize
  )
where

import Control.Exception              (IOException, evaluate, try)
import Control.Monad.IO.Class         (MonadIO, liftIO)
import Control.Monad.Reader           (local)
import Data.Binary                    (Binary)
import Data.Map.Strict                (Map)
import Data.Map.Strict                qualified as Map
import Data.Maybe                     (fromMaybe, listToMaybe)
import Data.Set                       (Set)
import Data.Set                       qualified as Set
import GHC.Generics                   (Generic)
import Hyperion                       (Job, Static, cAp, cPure, setTaskCpus)
import Hyperion                       qualified as Hyp
import Hyperion.Log                   qualified as Log
import Hyperion.Scheduler.FilePath    (VirtualFilePath (VirtualFilePath))
import Hyperion.Scheduler.SchedulerHandle (SchedulerHandle)
import Hyperion.Scheduler.Task.IsTask (IsTask (..), taskOutputPaths)
import Hyperion.Scheduler.Types (FileSize (..), MemorySize (..), NumCPUs)
import Hyperion.Scheduler.WorkerPool  (TWorker, remoteRunOnNewWorker)
import System.Directory.OsPath        (getFileSize)
import System.RUsage                  qualified as RUsage

data RemoteRunTaskResult = MkRemoteRunTaskResult
  { remoteTaskMemory    :: Maybe MemorySize
  , remoteTaskFileSizes :: Map VirtualFilePath FileSize
  }
  deriving (Generic, Binary, Show)

instance Static (Binary Hyperion.Scheduler.RunTasks.RemoteRunTask.RemoteRunTaskResult) where
  closureDict = static Hyp.Dict

emptyRemoteRunTaskResult :: RemoteRunTaskResult
emptyRemoteRunTaskResult = MkRemoteRunTaskResult { remoteTaskMemory = Nothing, remoteTaskFileSizes = Map.empty }

-- | The peak resident set size on the @VmHWM@ line of a @/proc/<pid>/status@
-- text, in bytes; 'Nothing' if there is no such line.
parseVmHWM :: String -> Maybe MemorySize
parseVmHWM status = listToMaybe
  [ MemorySize (fromIntegral (1024 * kb))
  | line <- lines status
  , ("VmHWM:" : kbText : _) <- [words line]
  , (kb, "") <- reads kbText :: [(Integer, String)]
  ]

-- | Peak resident set size of this process, in bytes, from @VmHWM@ in
-- @/proc/self/status@; 'Nothing' where that file or line is missing (not
-- Linux).
--
-- Preferred over @ru_maxrss@ from 'RUsage.get': @VmHWM@ belongs to the
-- current program image and starts from zero at @exec@, whereas Linux folds
-- the pre-exec image's high-water mark into @ru_maxrss@, so a worker spawned
-- on the master's own node inherits the master's peak as a floor and every
-- task there reports at least the master's size (hyperion issue 3; the
-- @sh -c@ wrapper of hyperion 9cd4e92 does not help, because bash execs a
-- lone command without forking). Workers launched through srun or ssh are
-- unaffected either way.
peakResidentSetSize :: IO (Maybe MemorySize)
peakResidentSetSize = do
  result <- try $ do
    status <- readFile "/proc/self/status"
    _ <- evaluate (length status)
    pure status
  pure $ either (\(_ :: IOException) -> Nothing) parseVmHWM result

-- | Run the action, then report the memory the task used: the peak resident
-- set size of this worker (see 'peakResidentSetSize'; @ru_maxrss@ where that
-- is unavailable) or of its child processes, whichever is larger.
afterReturnMemoryM :: MonadIO m => m () -> m (Maybe MemorySize)
afterReturnMemoryM go = do
  go
  let kilobytesToBytes k = MemorySize (1024 * fromIntegral k)
  self      <- liftIO peakResidentSetSize
  rSelf     <- liftIO $ RUsage.get RUsage.Self
  rChildren <- liftIO $ RUsage.get RUsage.Children
  pure $ Just $ max
    (fromMaybe (kilobytesToBytes rSelf.maxResidentSetSize) self)
    (kilobytesToBytes rChildren.maxResidentSetSize)

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

-- | Run a task on the given worker with the given number of CPUs and the
-- task's 'SchedulerHandle'.
remoteRunTask :: IsTask a => Maybe TWorker -> NumCPUs -> SchedulerHandle -> a -> Job RemoteRunTaskResult
remoteRunTask mWorker numCpus handle task = case taskClosureWithHandle numCpus handle task of
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

