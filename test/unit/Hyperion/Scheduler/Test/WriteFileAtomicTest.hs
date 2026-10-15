-- | 'writeFileAtomic' leaves no temporary file behind, whether the write
-- succeeds, the write fails or the move fails. Uses a temporary directory.
module Hyperion.Scheduler.Test.WriteFileAtomicTest where

import Control.Exception        (AssertionFailed (..), ErrorCall (..),
                                 IOException, SomeException, throwIO, try)
import Control.Monad            (unless)
import Data.Either              (isLeft)
import Data.List                (sort)
import Hyperion.OsString        (fromString)
import Hyperion.Util.EncodeFile (writeFileAtomic)
import System.Directory         (createDirectory, getTemporaryDirectory,
                                 listDirectory, removeDirectoryRecursive)
import System.FilePath          ((</>))

expect :: String -> Bool -> IO ()
expect label cond = do
  unless cond $ throwIO $ AssertionFailed $ "FAILED: " <> label
  putStrLn $ "ok: " <> label

runTest :: IO ()
runTest = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "hyperion-scheduler-write-file-atomic-test"
  () <$ try @IOException (removeDirectoryRecursive dir)
  createDirectory dir
  let
    written = dir </> "written"
    failed = dir </> "failed"
    -- A file cannot replace a non-empty directory, so the move fails.
    blocked = dir </> "blocked"
  createDirectory blocked
  writeFile (blocked </> "inside") ""
  writeFileAtomic (`writeFile` "value") (fromString written)
  content <- readFile written
  writeFailure <- try @ErrorCall $ writeFileAtomic
    (\file -> writeFile file "partial" >> throwIO (ErrorCall "boom"))
    (fromString failed)
  moveFailure <- try @SomeException $
    writeFileAtomic (`writeFile` "value") (fromString blocked)
  files <- sort <$> listDirectory dir
  removeDirectoryRecursive dir
  expect "a written file has its content" $ content == "value"
  expect "a failed write rethrows its error" $
    either (== ErrorCall "boom") (const False) writeFailure
  expect "a failed move is an error" $ isLeft moveFailure
  expect "no temporary file is left behind" $ files == ["blocked", "written"]
  putStrLn "All WriteFileAtomic tests passed."
