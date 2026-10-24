{-# LANGUAGE OverloadedStrings #-}

module Hyperion.Scheduler.Test.StorageStrategyTest (runTest) where

import Control.Exception                               (AssertionFailed (..),
                                                        throwIO)
import Control.Monad                                   (unless)
import Hyperion.OsPath                                 (OsPath)
import Hyperion.Scheduler.PathResolver                 (PathResolver (..))
import Hyperion.Scheduler.PathResolver.StorageStrategy

-- | A key whose file goes to the given storage.
data TestKey = MkTestKey StorageType OsPath

data TestStrategy = MkTestStrategy

instance ToStorageType TestStrategy TestKey where
  toStorageType _ (MkTestKey storage _) = storage

instance ToRelativePath TestKey where
  toRelativePath (MkTestKey _ path) = path

expect :: String -> Bool -> IO ()
expect label cond = do
  unless cond $ throwIO $ AssertionFailed $ "FAILED: " <> label
  putStrLn $ "ok: " <> label

runTest :: IO ()
runTest = do
  let
    resolver = MkPathResolverWithStrategy
      { localDir        = "/local"
      , globalDir       = "/global"
      , persistentDir   = "/persistent"
      , storageStrategy = MkTestStrategy
      }
  expect "a local key is in the local directory" $
    resolvePath resolver (MkTestKey Local "a/x") == "/local/a/x"
  expect "a global key is in the global directory" $
    resolvePath resolver (MkTestKey Global "a/x") == "/global/a/x"
  expect "a persistent key is in the persistent directory" $
    resolvePath resolver (MkTestKey Persistent "a/x") == "/persistent/a/x"
  expect "a custom key is in its own directory" $
    resolvePath resolver (MkTestKey (Custom "/shared") "a/x") == "/shared/a/x"
  putStrLn "All StorageStrategy tests passed."
