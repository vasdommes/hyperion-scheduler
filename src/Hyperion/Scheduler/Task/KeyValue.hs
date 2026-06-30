{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Hyperion.Scheduler.Task.KeyValue
  ( ValueType,
    readValue,
    unlessValueFileExists,
    writeValue,
    encodeBinaryFileAtomic
  )
where

import Control.Monad.Extra          (unlessM)
import Control.Monad.IO.Class       (MonadIO, liftIO)
import Data.Binary                  (Binary)
import Data.Binary                  qualified as Binary
import Hyperion.Log                 qualified as Log
import Hyperion.OsString            qualified as OsString
import Hyperion.Scheduler           (PathResolver (..))
import Hyperion.Scheduler.Task.Util (encodeBinaryFileAtomic)
import System.Directory.OsPath      (doesFileExist)

type family ValueType k

readValue :: (PathResolver r k, Binary (ValueType k)) => r -> k -> IO (ValueType k)
readValue resolver key = do
  Log.info "Reading file" path
  Binary.decodeFile (OsString.toString path)
  where
    path = resolvePath resolver key

unlessValueFileExists :: (MonadIO m, PathResolver r k) => r -> k -> m () -> m ()
unlessValueFileExists resolver key =
  unlessM (liftIO $ doesFileExist $ resolvePath resolver key)

writeValue
  :: (PathResolver r k, Binary (ValueType k), Show k)
  => r
  -> k
  -> ValueType k
  -> IO ()
writeValue resolver key x = do
  Log.info "Saving key object to file" (key, path)
  encodeBinaryFileAtomic path x
  where
    path = resolvePath resolver key
