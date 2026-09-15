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
-- priority. The writer supplies each element's priority: the scheduler's
-- priorities depend on the task graph, which grows during a run, so the
-- queue does not compute them itself.
newtype TPrioQueue k a = MkTPrioQueue
  { queueVar :: TVar (MaxQueue.MaxPQueue k a)
  }

-- | Create an empty TPrioQueue.
new :: MonadIO m => m (TPrioQueue k a)
new = liftIO $ MkTPrioQueue <$> newTVarIO MaxQueue.empty

-- | Add an element with the given priority.
write :: (MonadIO m, Ord k) => TPrioQueue k a -> k -> a -> m ()
write (MkTPrioQueue qVar) k x =
  liftIO $ atomically $ modifyTVar qVar (MaxQueue.insert k x)

-- | Read the first element of a TPrioQueue and remove it from the
-- queue. Blocks if the queue is empty. Modeled on 'readTQueue'
read :: Ord k => TPrioQueue k a -> STM a
read (MkTPrioQueue qVar) = do
  queue <- readTVar qVar
  check $ not (MaxQueue.null queue)
  let ((_, x), queue') = MaxQueue.deleteFindMax queue
  writeTVar qVar queue'
  pure x

-- | Look at the first element of a TPrioQueue without removing it.
tryPeek :: TPrioQueue k a -> STM (Maybe a)
tryPeek (MkTPrioQueue qVar) = do
  queue <- readTVar qVar
  pure $ fmap snd $ MaxQueue.getMax queue
