{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module Hyperion.Scheduler.Task.Util where

import Data.ByteString         qualified as BS
import Data.Store              (Store)
import Data.Store              qualified as Store
import Hyperion.Log            qualified as Log
import Hyperion.OsPath         (OsPath, takeDirectory)
import Hyperion.OsString       (toString)
import Hyperion.Util           (randomOsString)
import System.Directory.OsPath (createDirectoryIfMissing, renameFile)

-- | Write a representation of 'x' to a temporary file, and then move the
-- temporary file into place.
encodeStoreFileAtomic :: Store a => OsPath -> a -> IO ()
encodeStoreFileAtomic path x = do
  salt <- randomOsString 6
  let tmpPath = path <> "_" <> salt
  Log.info "Writing Store file" path
  createDirectoryIfMissing True (takeDirectory path)
  BS.writeFile (toString tmpPath) (Store.encode x)
  renameFile tmpPath path

-- | Read a value written by 'encodeStoreFileAtomic'.
decodeStoreFile :: Store a => OsPath -> IO a
decodeStoreFile path = Store.decodeIO =<< BS.readFile (toString path)
