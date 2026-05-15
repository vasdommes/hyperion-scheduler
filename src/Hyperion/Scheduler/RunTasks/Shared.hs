{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE StaticPointers        #-}

module Hyperion.Scheduler.RunTasks.Shared where

import Control.Concurrent.Classy.RWLock (RWLock, newRWLock)
import Control.Concurrent.Classy.RWLock qualified as RWLock
import Control.Monad.Conc.Class         (IORef, MonadConc, newIORef, readIORef,
                                         writeIORef)


data Shared m a = MkShared
  {
    lock :: RWLock m,
    ref  :: IORef m a
  }

newShared :: (MonadConc m) => a -> m (Shared m a)
newShared x = do
  lock' :: RWLock m <- newRWLock
  ref' :: IORef m a <- newIORef x
  pure $ MkShared {lock = lock', ref = ref'}

-- NB: performing arbitrary IO under lock is dangerous!
withReadM :: (MonadConc m) => (Shared m a) -> (a -> m b) -> m b
withReadM (MkShared lock ref) go = RWLock.withRead lock $ do
  value <- readIORef ref
  go value

-- NB: performing arbitrary IO under lock is dangerous!
withWriteM :: (MonadConc m) => (Shared m a) -> (a -> m (a, b)) -> m b
withWriteM (MkShared lock ref) go = RWLock.withWrite lock $ do
  value <- readIORef ref
  (value', res) <- go value
  writeIORef ref value'
  pure res

withRead :: (MonadConc m) => (Shared m a) -> (a -> b) -> m b
withRead shared go = withReadM shared $ pure . go

withWrite :: (MonadConc m) => (Shared m a) -> (a -> (a, b)) -> m b
withWrite shared go = withWriteM shared $ pure . go

-- TODO rename to modifyShared?
withWrite_ :: (MonadConc m) => (Shared m a) -> (a -> a) -> m ()
withWrite_ shared go = withWrite shared go' where
  go' x = (go x, ())

readShared :: (MonadConc m) => (Shared m a) -> m a
readShared = flip withRead id
