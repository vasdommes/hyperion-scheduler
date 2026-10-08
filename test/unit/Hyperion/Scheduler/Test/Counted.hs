-- | Counting how often a value is computed, to test that work is shared.
module Hyperion.Scheduler.Test.Counted where

import Data.IORef       (IORef, atomicModifyIORef', newIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

-- | A counter. Bind it at the top level with @NOINLINE@, so that there is one.
newCounter :: IO (IORef Int)
newCounter = newIORef 0

resetCounter :: IORef Int -> IO ()
resetCounter counter = writeIORef counter 0

-- | @x@, counting each evaluation. @dep@ is what @x@ is computed from: it ties
-- each call to its arguments, so that the optimizer cannot float a call out
-- of its function and share one evaluation between calls.
counted :: IORef Int -> dep -> a -> a
counted counter dep x = unsafePerformIO $ do
  atomicModifyIORef' counter (\n -> (n + 1, ()))
  pure (dep `seq` x)
{-# NOINLINE counted #-}
