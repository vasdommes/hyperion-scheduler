{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Hyperion.Scheduler.TPrioQueue where

import Control.Concurrent.STM (STM, TVar, atomically, check, modifyTVar,
                               newTVarIO, readTVar, writeTVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.PQueue.Prio.Max   qualified as MaxQueue
import Prelude                hiding (read)

-- | An STM-based priority queue. Values are sorted from high to low
-- priority.
data TPrioQueue k a = MkTPrioQueue
  { queueVar     :: TVar (MaxQueue.MaxPQueue k a)
  , elemPriority :: a -> k
  }

-- | Create a new TPrioQueue with the given priority function.
new :: MonadIO m => (a -> k) -> m (TPrioQueue k a)
new prio = liftIO $ do
  qVar <- newTVarIO MaxQueue.empty
  pure (MkTPrioQueue qVar prio)

-- | Add an element to the TPrioQueue.
write :: (MonadIO m, Ord k) => TPrioQueue k a -> a -> m ()
write (MkTPrioQueue qVar prio) x =
  liftIO $ atomically $ modifyTVar qVar (MaxQueue.insert (prio x) x)

-- | Read the first element of a TPrioQueue and remove it from the
-- queue. Blocks if the queue is empty. Modeled on 'readTQueue'
read :: Ord k => TPrioQueue k a -> STM a
read (MkTPrioQueue qVar _) = do
  queue <- readTVar qVar
  check $ not (MaxQueue.null queue)
  let ((_, x), queue') = MaxQueue.deleteFindMax queue
  writeTVar qVar queue'
  pure x

-- | Return the first element of a TPrioQueue, or Nothing if
-- the queue is empty. Does not block. Modeled on 'tryPeekTQueue'.
tryPeek :: TPrioQueue k a -> STM (Maybe a)
tryPeek (MkTPrioQueue qVar _) = do
  queue <- readTVar qVar
  pure $ fmap snd $ MaxQueue.getMax queue
