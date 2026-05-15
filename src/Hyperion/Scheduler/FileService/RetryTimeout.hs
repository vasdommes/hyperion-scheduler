{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot        #-}
{-# LANGUAGE StaticPointers             #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Hyperion.Scheduler.FileService.RetryTimeout where

import Control.Distributed.Process       (Process)
import Control.Distributed.Process.Async (AsyncResult (..), asyncLinked, task,
                                          wait, waitCancelTimeout)
import Data.Binary                       (Binary)
import Data.Time.Clock                   (NominalDiffTime)
import Data.Typeable                     (Typeable)
import GHC.Generics                      (Generic)
import Hyperion                          ()
import Hyperion.Util                     (nominalDiffTimeToMicroseconds)


-- Retry policy with exponentially increasing timeout.
-- NB:
-- Note that Control.Retry
data RetryTimeoutPolicy = MkRetryTimeoutPolicy
  { maxRetryCount  :: Int
  , initialTimeout :: Maybe NominalDiffTime
  }
  deriving (Generic, Binary, Show)

nextRetryTimeoutPolicy :: RetryTimeoutPolicy -> Maybe RetryTimeoutPolicy
nextRetryTimeoutPolicy policy =
  if policy.maxRetryCount == 0 then
    Nothing
  else
    Just policy
    { maxRetryCount = policy.maxRetryCount - 1
    -- Exponential backoff
    , initialTimeout = (* 2) <$> policy.initialTimeout
    }

-- Run task, retry if timeout exceeded.
runWithRetryTimeout
  :: ( Binary a
     , Typeable a
     )
  => RetryTimeoutPolicy
  -> (AsyncResult a -> RetryTimeoutPolicy ->  Process ())
  -> Process a
  -> Process (AsyncResult a)
runWithRetryTimeout initialPolicy beforeRetry action = do
  let
    go policy = do
      asyncHandle <- asyncLinked $ task action
      asyncResult <- case policy.initialTimeout of
        Just timeout -> waitCancelTimeout (nominalDiffTimeToMicroseconds timeout) asyncHandle
        Nothing      -> wait asyncHandle
      case asyncResult of
        AsyncDone _ -> return asyncResult
        _ -> do
          case nextRetryTimeoutPolicy policy of
            Just policy' -> do
              beforeRetry asyncResult policy'
              go policy'
            Nothing -> return asyncResult

  go initialPolicy
