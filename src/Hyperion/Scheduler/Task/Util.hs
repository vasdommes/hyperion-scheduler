{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module Hyperion.Scheduler.Task.Util
  ( contramapKey
  , emptyTaskChain
  , unvariant
  , vEither
  , vNil
  , vAll
  , encodeBinaryFileAtomic
  ) where

import Bootstrap.Build         (All)
import Bootstrap.Build.FList   (Variant (..))
import Data.Binary             (Binary)
import Data.Binary             qualified as Binary
import Data.Set                qualified as Set
import Data.Void               (Void)
import Hyperion.Log            qualified as Log
import Hyperion.OsPath         (OsPath, takeDirectory)
import Hyperion.OsString       (toString)
import Hyperion.Scheduler      (TaskChain (..), TaskLink (..))
import Hyperion.Util           (randomOsString)
import System.Directory.OsPath (createDirectoryIfMissing, renameFile)

-- TODO: Move to Scheduler
contramapKey :: (k' -> k) -> TaskLink m k d t -> TaskLink m k' d t
contramapKey f taskLink =
  MkTaskLink
    { dependencies = taskLink.dependencies . f
    , checkCreated = taskLink.checkCreated . f
    , toTask       = taskLink.toTask . f
    }

-- TODO: Move to Bootstrap.Build
vNil :: Variant '[] -> a
vNil _ = error "absurd"

-- TODO: Move to Bootstrap.Build
vEither :: (a -> c) -> (Variant as -> c) -> Variant (a ': as) -> c
vEither f _ (VLeft x)   = f x
vEither _ vf (VRight y) = vf y

-- TODO: Move to Bootstrap.Build
vAll :: forall c ks b . All c ks => (forall a. c a => a -> b) -> Variant ks -> b
vAll f (VLeft x)  = f x
vAll f (VRight y) = vAll @c f y

-- TODO: Move to Bootstrap.Build
unvariant :: Variant '[a] -> a
unvariant = id `vEither` vNil

-- Move to Scheduler
emptyTaskChain :: Applicative m => TaskChain m (Variant '[]) t
emptyTaskChain = TaskNode emptyTaskLink TaskNil

-- Move to Scheduler
emptyTaskLink :: Applicative m => TaskLink m (Variant '[]) Void t
emptyTaskLink = MkTaskLink
  { dependencies = const Set.empty
  , checkCreated = const (pure True)
  , toTask       = error "absurd"
  }

-- Copied from SDPB.Write, TODO move elsewhere
-- | Write a json representation of 'x' to a temporary file, and then
-- move the temporary file into place.
encodeBinaryFileAtomic :: Binary a => OsPath -> a -> IO ()
encodeBinaryFileAtomic path x = do
  salt <- randomOsString 6
  let tmpPath = path <> "_" <> salt
  Log.info "Writing Binary file" path
  createDirectoryIfMissing True (takeDirectory path)
  Binary.encodeFile (toString tmpPath) x
  renameFile tmpPath path
