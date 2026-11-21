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

import Control.Exception              (throwIO)
import Control.Monad.Catch            (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class         (MonadIO, liftIO)
import Control.Monad.Reader           (ReaderT, ask, runReaderT)
import Control.Monad.Trans            (MonadTrans (..))
import Data.ByteString.Short          (useAsCString)
import Data.IORef                     (IORef, atomicModifyIORef', newIORef,
                                       readIORef)
import Data.Int                       (Int64)
import Data.Map.Strict                (Map)
import Data.Map.Strict                qualified as Map
import Data.Maybe                     (isJust)
import Foreign.C.Error                (errnoToIOError, getErrno)
import Foreign.C.String               (CString)
import Foreign.C.Types                (CInt (..))
import Foreign.Marshal.Alloc          (alloca)
import Foreign.Ptr                    (Ptr)
import Foreign.Storable               (peek)
import Hyperion                       (Dict (..), Static (..))
import Hyperion.OsPath                (OsPath)
import Hyperion.Scheduler.Config      (Config)
import Hyperion.Scheduler.FilePath    (isLocalPath)
import Hyperion.Scheduler.Types       (FileSize)
import System.IO.Error                (isDoesNotExistError)
import System.OsString.Internal.Types (OsString (..), PosixString (..))

class Monad m => MonadTaskFiles m where
  -- | The size of the regular file at the path, 'Nothing' if nothing is
  -- there. Throws if something else is there, e.g. a directory.
  taskFileSize :: OsPath -> m (Maybe FileSize)

-- | Whether the task file exists. See 'taskFileSize'.
doesTaskFileExist :: MonadTaskFiles m => OsPath -> m Bool
doesTaskFileExist = fmap isJust . taskFileSize

-- | 'taskFileSize' with one statx call, following symlinks. The call need not
-- reach the servers that hold the file's data (@AT_STATX_DONT_SYNC@, see
-- @cbits/task_file_size.c@): on Lustre, a plain stat fetches the size from
-- them, which cost 10-70 ms a file on Expanse's scratch, and minutes for the
-- thousands of files a task map checks (bootstrap-compute #1344). Existence and
-- type are exact; the size is the file's size at its last close.
statTaskFile :: OsPath -> IO (Maybe FileSize)
statTaskFile path =
  useAsCString (getPosixString (getOsString path)) $ \cPath ->
  alloca $ \sizePtr -> do
    r <- c_taskFileSize cPath sizePtr
    case r of
      1 -> Just . fromIntegral <$> peek sizePtr
      0 -> ioError $ userError $ "Task file is not a regular file: " <> show path
      _ -> do
        errno <- getErrno
        let e = errnoToIOError "statTaskFile" errno Nothing (Just (show path))
        if isDoesNotExistError e then pure Nothing else throwIO e

foreign import ccall safe "hyperion_scheduler_task_file_size"
  c_taskFileSize :: CString -> Ptr Int64 -> IO CInt

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
