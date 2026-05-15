{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StaticPointers      #-}
{-# LANGUAGE TypeFamilies        #-}

module Hyperion.Scheduler.Task
  ( Task
  , mkTask
  , mkTaskWithInfo
  , mkTaskWithStats
  , HasTaskHash(..)
  , DummyTask(..)
  , unDummyTask
  , afterReturnRemoteRunTaskResult
  ) where

import Control.Concurrent                 (threadDelay)
import Control.Monad.IO.Class             (MonadIO, liftIO)
import Data.Aeson                         (ToJSON (..))
import Data.Binary                        (Binary (..))
import Data.BinaryHash                    (hashBase64SafeByteString)
import Data.ByteString                    (ByteString)
import Data.ByteString.Char8              qualified as ByteString
import Data.Map                           qualified as Map
import Data.Maybe                         (fromMaybe)
import Data.Set                           (Set)
import Data.Set                           qualified as Set
import Data.Text                          qualified as Text
import GHC.Generics                       (Generic)
import Hyperion                           (Closure, Process, cAp, cPure)
import Hyperion.Scheduler.FilePath        (VirtualFilePath (..))
import Hyperion.Scheduler.RunTasks        (CanRemoteRunTask (..),
                                           RemoteRunTaskResult (..),
                                           emptyRemoteRunTaskResult)
import Hyperion.Scheduler.Stats           (TaskAndFileStats, ToStatKey (..),
                                           approxRuntime, lookupMaxFileSize,
                                           lookupTaskStats, maxMemory)
import Hyperion.Scheduler.TaskInfo        (HasTaskInfo (..), TaskInfo (..),
                                           taskInputs, taskMemory,
                                           taskOutputPaths, taskOutputs,
                                           taskRuntime)
import Hyperion.Scheduler.TaskKeyFileInfo (TaskKeyFileInfo (..))
import Hyperion.Scheduler.Types           (MemorySize (..))
import Hyperion.Util                      (nominalDiffTimeToMicroseconds)
import System.Directory.OsPath            (getFileSize)
import System.RUsage                      qualified as RUsage
import Type.Reflection                    (Typeable)

-- | A general container for an instance of HasTaskInfo and
-- CanRemoteRunTask. We include a ByteString hash for quick
-- comparisons, so that data structures like Set's and Map's are
-- reasonably performant.
--
-- WARNING: If two different tasks ever have the same hash, this could
-- lead to undefined behavior.
data Task where
  MkTask :: (Typeable a, CanRemoteRunTask a, ToJSON a, ToStatKey a) => ByteString -> TaskInfo -> a -> Task

instance Eq Task where
  MkTask h1 _ _ == MkTask h2 _ _ = h1 == h2

instance Ord Task where
  compare (MkTask h1 _ _) (MkTask h2 _ _) = compare h1 h2

instance Show Task where
  show (MkTask h i _) = concat
    [ "MkTask "
    , case i.tag of
        Nothing -> "Nothing"
        Just t' -> Text.unpack t'
    , " "
    , show (i.runtime 1)
    , " \""
    , ByteString.unpack h
    , "\""
    ]

instance ToJSON Task where
  toJSON (MkTask _ _ t) = toJSON t

instance HasTaskInfo Task where
  taskInfo (MkTask _ i _) = i

instance ToStatKey Task where
  toStatKey (MkTask _ _ t) = toStatKey t

instance CanRemoteRunTask Task where
  remoteRunTask node numCpus (MkTask _ _ t) = remoteRunTask node numCpus t

class HasTaskHash a where
  taskHash :: a -> ByteString
  default taskHash :: (Typeable a, Binary a) => a -> ByteString
  taskHash = hashBase64SafeByteString

instance HasTaskHash ()

-- | A smart constructor for a Task.
mkTask :: (HasTaskInfo a, CanRemoteRunTask a, Typeable a, HasTaskHash a, ToJSON a, ToStatKey a) => a -> Task
mkTask t = mkTaskWithInfo (taskInfo t) t

-- | A smart constructor for a Task.
mkTaskWithInfo :: (CanRemoteRunTask a, Typeable a, HasTaskHash a, ToJSON a, ToStatKey a) => TaskInfo -> a -> Task
mkTaskWithInfo i t = MkTask (taskHash t) i t

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

afterReturnRemoteRunTaskResult
  :: HasTaskInfo a
  => a
  -> Closure (Process ())
  -> Closure (Process RemoteRunTaskResult)
afterReturnRemoteRunTaskResult task go =
  static afterReturnRemoteRunTaskResultM
  -- TODO: measure input file sizes too?
  `cAp` cPure (taskOutputPaths task)
  `cAp` go


-- | Create a task whose memory and runtime are estimted with the
-- given 'TaskResourceMap's.
mkTaskWithStats
  :: (HasTaskInfo a, CanRemoteRunTask a, Typeable a, HasTaskHash a, ToJSON a, ToStatKey a)
  => TaskAndFileStats
  -> a
  -> Task
mkTaskWithStats stats task = mkTaskWithInfo info task
  where
    maybeTaskResourceMap = lookupTaskStats task stats
    runtime = fromMaybe (taskRuntime task) (maybeTaskResourceMap >>= approxRuntime Nothing)
    memory  = fromMaybe (taskMemory task)  (maybeTaskResourceMap >>= maxMemory)

    updateFileSize fileInfo = fileInfo { fileSize = fileSize} where
      fileSize = fromMaybe fileInfo.fileSize $ lookupMaxFileSize (fileInfo.fileStatKey) stats
    inputs = Set.map updateFileSize $ taskInputs task
    outputs = Set.map updateFileSize $ taskOutputs task

    info = (taskInfo task)
      { runtime = runtime
      , memory  = memory
      , inputs  = inputs
      , outputs = outputs
      }

-- | A task with a trivial 'CanRemoteRunTask' instance that just does
-- a threadDelay, sped up by the given Int factor. This is for testing
-- purposes.
data DummyTask a = MkDummyTask Int a
  deriving (Eq, Ord, Show, Generic, ToJSON)

unDummyTask :: DummyTask a -> a
unDummyTask (MkDummyTask _ x) = x

instance HasTaskHash a => HasTaskHash (DummyTask a) where
  taskHash = taskHash . unDummyTask

instance HasTaskInfo a => HasTaskInfo (DummyTask a) where
  taskInfo = taskInfo . unDummyTask

instance ToStatKey a => ToStatKey (DummyTask a) where
  toStatKey = toStatKey . unDummyTask

instance HasTaskInfo a => CanRemoteRunTask (DummyTask a) where
  remoteRunTask _ numCpus (MkDummyTask speedup t) = do
    liftIO $ threadDelay (nominalDiffTimeToMicroseconds (taskRuntime t numCpus) `div` speedup)
    pure $ emptyRemoteRunTaskResult { remoteTaskMemory = Just (taskMemory t) }
