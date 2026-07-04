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
import Data.Typeable      (Typeable)
import Hyperion.Scheduler (IsTask (..), TaskLink (..), ToStatKey (..))

-- TODO: This is general and can be moved to the scheduler
newtype ListTask k = MkListTask { keys :: [k] }
  deriving newtype (Binary, ToJSON, Eq, Ord)

instance (Eq k, Ord k, ToJSON k, Typeable k) => IsTask (ListTask k) where
  taskMaxThreads _ _ = 0
  taskMinThreads _ _ = 0
  taskInputs _       = Set.empty
  taskOutputs _      = Set.empty
  taskTag _          = Nothing
  taskClosure _ _    = Nothing

instance ToStatKey (ListTask k) where
  toStatKey _ = toStatKey ()

listTaskLink :: (Ord k, Applicative m) => TaskLink m [k] k (ListTask k)
listTaskLink = MkTaskLink
  { dependencies = Set.fromList
  , checkCreated = const (pure False)
  , toTask = MkListTask
  }
