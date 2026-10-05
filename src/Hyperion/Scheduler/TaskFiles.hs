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
import Control.Monad.IO.Class         (MonadIO, liftIO)
import Control.Monad.Reader           (ReaderT, ask, lift, runReaderT)
import Data.IORef                     (IORef, atomicModifyIORef', newIORef,
                                       readIORef)
import Data.Map.Strict                (Map)
import Data.Map.Strict                qualified as Map
import Data.Maybe                     (isJust)
import Hyperion                       (Dict (..), Static (..))
import Hyperion.OsPath                (OsPath)
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

instance {-# OVERLAPPABLE #-} (Monad m, MonadIO m) => MonadTaskFiles m where
  taskFileSize = liftIO . statTaskFile

-- | 'taskFileSize' with one stat call, following symlinks.
statTaskFile :: OsPath -> IO (Maybe FileSize)
statTaskFile path = try @IOException (Posix.getFileStatus (getOsString path)) >>= \case
  Left e
    | isDoesNotExistError e -> pure Nothing
    | otherwise             -> throwIO e
  Right status
    | Posix.isRegularFile status -> pure $ Just $ fromIntegral $ Posix.fileSize status
    | otherwise -> ioError $ userError $ "Task file is not a regular file: " <> show path

data MemoizedTaskFilesEnv = MkMemoizedTaskFilesEnv
  { knownAbsent :: OsPath -> Bool
    -- ^ Paths answered as absent without the filesystem.
  , sizesRef    :: IORef (Map OsPath (Maybe FileSize))
  }

-- | 'MonadTaskFiles' that stats each path once, for building task maps.
newtype MemoizedTaskFiles a = MkMemoizedTaskFiles (ReaderT MemoizedTaskFilesEnv IO a)
  deriving newtype (Functor, Applicative, Monad)

instance MonadTaskFiles MemoizedTaskFiles where
  taskFileSize path = MkMemoizedTaskFiles $ do
    env <- ask
    if env.knownAbsent path then pure Nothing else lift $ do
      sizes <- readIORef env.sizesRef
      case Map.lookup path sizes of
        Just size -> pure size
        Nothing -> do
          size <- statTaskFile path
          atomicModifyIORef' env.sizesRef (\m -> (Map.insert path size m, ()))
          pure size

instance Static (MonadTaskFiles MemoizedTaskFiles) where
  closureDict = static Dict

runMemoizedTaskFiles :: MonadIO m => MemoizedTaskFiles a -> m a
runMemoizedTaskFiles = runMemoizedTaskFilesWith (const False)

-- | 'runMemoizedTaskFiles', answering the paths for which @knownAbsent@
-- holds as absent without the filesystem.
runMemoizedTaskFilesWith :: MonadIO m => (OsPath -> Bool) -> MemoizedTaskFiles a -> m a
runMemoizedTaskFilesWith knownAbsent (MkMemoizedTaskFiles m) = liftIO $ do
  sizesRef <- newIORef Map.empty
  runReaderT m MkMemoizedTaskFilesEnv { knownAbsent, sizesRef }
