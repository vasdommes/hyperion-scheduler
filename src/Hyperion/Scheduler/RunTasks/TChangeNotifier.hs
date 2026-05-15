{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE StaticPointers        #-}

module Hyperion.Scheduler.RunTasks.TChangeNotifier
where

import Control.Concurrent.STM (STM, TVar, atomically, check, modifyTVar',
                               newTVarIO, readTVar, readTVarIO)
import Control.Monad.IO.Class (MonadIO, liftIO)

type TChangeNotifier = TVar Integer

newChangeNotifierIO :: IO TChangeNotifier
newChangeNotifierIO = newTVarIO 0

-- Each notifyChange increases counter by 1
notifyChange :: TChangeNotifier -> STM ()
notifyChange notifier = modifyTVar' notifier succ

notifyChangeM :: (MonadIO m) => TChangeNotifier -> m ()
notifyChangeM = liftIO . atomically . notifyChange

waitChange :: (Eq a) => TVar a -> a -> STM a
waitChange tVar oldValue = do
  newValue <- readTVar tVar
  check $ newValue /= oldValue
  return newValue

-- | `runWithRetry notifier go` runs `go`, which returns (Maybe a).
-- In case of `Just a` we return `a`.
-- In case of `Nothing`, we wait for `notifier` changes and retry.
-- This resembles `Control.Concurrent.STM.retry` logic,
-- but with custom `TChangeNotifier` and outside of `STM` monad.
runWithRetry :: MonadIO m => TChangeNotifier -> m (Maybe a) -> m a
runWithRetry notifier go = do
  let
    go' oldValue = do
      maybeRes <- go
      case maybeRes of
        Just res -> return res
        Nothing  -> do
          newValue <- liftIO $ atomically $ waitChange notifier oldValue
          go' newValue
  (liftIO $ readTVarIO notifier) >>= go'
