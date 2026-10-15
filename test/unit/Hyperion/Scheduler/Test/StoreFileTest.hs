-- | 'decodeStoreFile' reads what 'encodeStoreFileAtomic' writes. A decode
-- error names the file; other exceptions pass through unchanged. Uses a
-- temporary directory.
module Hyperion.Scheduler.Test.StoreFileTest where

import Control.Exception        (AssertionFailed (..), Exception, IOException,
                                 SomeException, fromException, throw, throwIO,
                                 try)
import Control.Monad            (unless)
import Data.List                (isInfixOf)
import Data.Store               (Size (..), Store (..))
import Hyperion.OsString        (fromString)
import Hyperion.Util.EncodeFile (decodeStoreFile, encodeStoreFileAtomic)
import System.Directory         (createDirectory, getTemporaryDirectory,
                                 removeDirectoryRecursive)
import System.FilePath          ((</>))

-- | An 'Int' whose decoding calls 'error'.
newtype Failing = MkFailing Int

instance Store Failing where
  size = ConstSize 8
  poke (MkFailing n) = poke n
  peek = error "bad value"

data Custom = MkCustom
  deriving (Eq, Show)

instance Exception Custom

-- | An 'Int' whose decoding throws 'Custom', like a kill arriving mid-read.
newtype Throwing = MkThrowing Int

instance Store Throwing where
  size = ConstSize 8
  poke (MkThrowing n) = poke n
  peek = throw MkCustom

expect :: String -> Bool -> IO ()
expect label cond = do
  unless cond $ throwIO $ AssertionFailed $ "FAILED: " <> label
  putStrLn $ "ok: " <> label

runTest :: IO ()
runTest = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "hyperion-scheduler-store-file-test"
  () <$ try @IOException (removeDirectoryRecursive dir)
  createDirectory dir
  let
    stored = dir </> "stored"
    truncated = dir </> "truncated"
  encodeStoreFileAtomic (fromString stored) (12345 :: Int)
  writeFile truncated "abc"
  roundTrip <- decodeStoreFile @Int (fromString stored)
  fromTruncated <-
    try @IOException $ decodeStoreFile @Int (fromString truncated)
  fromFailing <- try @IOException $ decodeStoreFile @Failing (fromString stored)
  fromThrowing <-
    try @SomeException $ decodeStoreFile @Throwing (fromString stored)
  removeDirectoryRecursive dir
  expect "a stored value reads back" $ roundTrip == 12345
  expect "a truncated payload is rejected, naming the file" $
    namesFile truncated fromTruncated
  expect "an error in decoding names the file" $ namesFile stored fromFailing
  expect "another exception passes through unchanged" $
    either ((== Just MkCustom) . fromException) (const False) fromThrowing
  putStrLn "All StoreFile tests passed."
  where
    namesFile file = either ((file `isInfixOf`) . show) (const False)
