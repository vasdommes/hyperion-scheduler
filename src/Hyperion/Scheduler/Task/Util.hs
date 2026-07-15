{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module Hyperion.Scheduler.Task.Util where

import Data.Binary             (Binary)
import Data.Binary             qualified as Binary
import Hyperion.Log            qualified as Log
import Hyperion.OsPath         (OsPath, takeDirectory)
import Hyperion.OsString       (toString)
import Hyperion.Util           (randomOsString)
import System.Directory.OsPath (createDirectoryIfMissing, renameFile)

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
