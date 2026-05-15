{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StaticPointers        #-}

-- | Convenience functions for ConcurrentQueue based on TQueue
-- TODO: Control.Concurrent.Chan.Unagi performs better under contention
-- compared to TQueue or Chan.
-- But Unagi.Chan has destructive tryReadChan, which means that
-- implementing tryReadQueue/flushQueue is very non-trivial:
-- one has to store pending Element from (tryReadChan outChan), if (tryRead element) is Nothing.
-- On each subsequent readQueue, one has to repeat (tryRead element) until it return (Just x).
-- Otherwise, this element will be lost and readChan will read the next one.
module Hyperion.Scheduler.ConcurrentQueue where

import Control.Concurrent.STM        (atomically)
import Control.Concurrent.STM.TQueue
import Control.Monad.Trans           (MonadIO, liftIO)
import Data.List.NonEmpty            (NonEmpty (..))

type ConcurrentQueue a = TQueue a

newQueue :: (MonadIO m) => m (ConcurrentQueue a)
newQueue = liftIO newTQueueIO

readQueue :: (MonadIO m) => ConcurrentQueue a -> m a
readQueue = liftIO . atomically . readTQueue

writeQueue :: (MonadIO m) => ConcurrentQueue a -> a -> m ()
writeQueue q = liftIO . atomically . writeTQueue q

writeListQueue :: (MonadIO m) => ConcurrentQueue a -> [a] -> m ()
writeListQueue q = liftIO . atomically . mapM_ (writeTQueue q)

flushQueue :: (MonadIO m) => ConcurrentQueue a -> m [a]
flushQueue = liftIO . atomically . flushTQueue

-- | Block until an element is available, and then read as
-- many elements as possible from the queue, returning a NonEmpty list.
readAndFlushQueue :: (MonadIO m) => ConcurrentQueue a -> m (NonEmpty a)
readAndFlushQueue q = liftIO $ atomically $ do
  firstElement <- readTQueue q
  restElements <- flushTQueue q
  pure $ firstElement :| restElements
