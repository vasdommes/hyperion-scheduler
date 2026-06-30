{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoFieldSelectors           #-}

module Hyperion.Scheduler.Task.ListTask
  ( ListTask (..)
  , listTaskLink
  ) where

import Data.Aeson         (ToJSON)
import Data.Binary        (Binary)
import Data.Set           qualified as Set
import Hyperion.Scheduler (CanRemoteRunTask (..), HasTaskHash, HasTaskInfo (..),
                           TaskInfo (..), TaskLink (..), ToStatKey (..),
                           emptyRemoteRunTaskResult)

-- TODO: This is general and can be moved to the scheduler
newtype ListTask k = MkListTask { keys :: [k] }
  deriving newtype (Binary, ToJSON)
  deriving anyclass (HasTaskHash)

instance HasTaskInfo (ListTask k) where
  taskInfo _ = MkTaskInfo
    { memory     = 0
    , runtime    = const 0
    , maxThreads = const 0
    , minThreads = const 0
    , inputs     = Set.empty
    , outputs    = Set.empty
    , priority   = 0
    , tag        = Nothing
    }

instance CanRemoteRunTask (ListTask k) where
  remoteRunTask _ _ _ = pure emptyRemoteRunTaskResult

instance ToStatKey (ListTask k) where
  toStatKey _ = toStatKey ()

listTaskLink :: (Ord k, Applicative m) => TaskLink m [k] k (ListTask k)
listTaskLink = MkTaskLink
  { dependencies = Set.fromList
  , checkCreated = const (pure False)
  , toTask = MkListTask
  }
