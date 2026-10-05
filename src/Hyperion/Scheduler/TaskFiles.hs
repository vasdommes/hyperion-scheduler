{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE StaticPointers        #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Queries about task files on disk. A task's input and output files are
-- regular files, never directories.
module Hyperion.Scheduler.TaskFiles where

import Control.Exception              (IOException, throwIO, try)
import Control.Monad.Catch            (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class         (MonadIO, liftIO)
import Control.Monad.Reader           (ReaderT, ask, runReaderT)
import Control.Monad.Trans            (MonadTrans (..))
import Data.IORef                     (IORef, atomicModifyIORef', newIORef,
                                       readIORef)
import Data.Map.Strict                (Map)
import Data.Map.Strict                qualified as Map
import Data.Maybe                     (isJust)
import Hyperion                       (Dict (..), Static (..))
import Hyperion.OsPath                (OsPath)
import Hyperion.Scheduler.Config      (Config (..))
import Hyperion.Scheduler.Types       (FileSize)
import System.IO.Error                (isDoesNotExistError)
import System.OsString.Internal.Types (OsString (..))
import System.Posix.Files.PosixString qualified as Posix

class Monad m => MonadTaskFiles m where
  -- | The size of the regular file at the path, 'Nothing' if nothing is
  -- there. Throws if something else is there, e.g. a directory.
  taskFileSize :: OsPath -> m (Maybe FileSize)

-- | Whether the task file exists. See 'taskFileSize'.
doesTaskFileExist :: MonadTaskFiles m => OsPath -> m Bool
doesTaskFileExist = fmap isJust . taskFileSize

-- | 'taskFileSize' with one stat call, following symlinks.
statTaskFile :: OsPath -> IO (Maybe FileSize)
statTaskFile path =
  try @IOException (Posix.getFileStatus (getOsString path)) >>= \case
    Left e
      | isDoesNotExistError e -> pure Nothing
      | otherwise             -> throwIO e
    Right status
      | Posix.isRegularFile status ->
          pure $ Just $ fromIntegral $ Posix.fileSize status
      | otherwise ->
          ioError $ userError $ "Task file is not a regular file: " <> show path

-- | Task files as the scheduler sees them while building a task map: each path
-- is stat'ed once, and node-local paths are absent. Such a file from before
-- the run is unknown to the file service, which could not hand it to a
-- consumer; the filesystem would only show the building node's local storage.
newtype TaskFilesT m a = MkTaskFilesT (ReaderT TaskFilesEnv m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail, MonadThrow,
                    MonadCatch, MonadMask)

data TaskFilesEnv = MkTaskFilesEnv
  { knownAbsent :: OsPath -> Bool
    -- ^ Paths answered as absent without the filesystem.
  , sizesRef    :: IORef (Map OsPath (Maybe FileSize))
  }

instance MonadTrans TaskFilesT where
  lift = MkTaskFilesT . lift

instance MonadIO m => MonadTaskFiles (TaskFilesT m) where
  taskFileSize path = MkTaskFilesT $ do
    env <- ask
    if env.knownAbsent path then pure Nothing else liftIO $ do
      sizes <- readIORef env.sizesRef
      case Map.lookup path sizes of
        Just size -> pure size
        Nothing -> do
          size <- statTaskFile path
          atomicModifyIORef' env.sizesRef (\m -> (Map.insert path size m, ()))
          pure size

instance Static (MonadTaskFiles (TaskFilesT IO)) where
  closureDict = static Dict

-- | Build task maps with the task files of the scheduler with this config.
runTaskFiles :: MonadIO m => Config -> TaskFilesT m a -> m a
runTaskFiles config = runTaskFilesWith (isLocalPath config)

-- | 'runTaskFiles', answering the paths for which @knownAbsent@ holds as
-- absent. Prefer 'runTaskFiles': a node-local path must be absent.
runTaskFilesWith :: MonadIO m => (OsPath -> Bool) -> TaskFilesT m a -> m a
runTaskFilesWith knownAbsent (MkTaskFilesT m) = do
  sizesRef <- liftIO $ newIORef Map.empty
  runReaderT m MkTaskFilesEnv { knownAbsent, sizesRef }
