{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module Hyperion.Scheduler.Task.Util where

import Bootstrap.Build         (All)
import Bootstrap.Build.FList   (Variant (..))
import Data.Binary             (Binary)
import Data.Binary             qualified as Binary
import Hyperion.Log            qualified as Log
import Hyperion.OsPath         (OsPath, takeDirectory)
import Hyperion.OsString       (toString)
import Hyperion.Util           (randomOsString)
import System.Directory.OsPath (createDirectoryIfMissing, renameFile)

-- These 4 utilities should move to Bootstrap.Build. We probably want to make a
-- separate Variant module in that library instead of having it all in FList.

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
