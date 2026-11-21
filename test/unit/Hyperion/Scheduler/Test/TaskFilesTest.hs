-- | What 'statTaskFile' reports for a regular file, an empty one, a missing
-- path, a path under a regular file, a directory and symbolic links, and what
-- 'runTaskFilesWith' remembers and treats as absent.
-- Uses a temporary directory.
module Hyperion.Scheduler.Test.TaskFilesTest where

import Control.Exception            (AssertionFailed (..), IOException, throwIO,
                                     try)
import Control.Monad                (unless)
import Control.Monad.IO.Class       (liftIO)
import Data.Either                  (isLeft)
import Hyperion.OsString            (fromString)
import Hyperion.Scheduler.TaskFiles (MonadTaskFiles (..), runTaskFilesWith,
                                     statTaskFile)
import System.Directory             (createDirectory, createFileLink,
                                     getTemporaryDirectory,
                                     removeDirectoryRecursive, removeFile)
import System.FilePath              ((</>))

expect :: String -> Bool -> IO ()
expect label cond = do
  unless cond $ throwIO $ AssertionFailed $ "FAILED: " <> label
  putStrLn $ "ok: " <> label

runTest :: IO ()
runTest = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "hyperion-scheduler-task-files-test"
  removeIfPresent dir
  createDirectory dir
  writeFile (dir </> "file") (replicate 5000 'x')
  fileSize <- statTaskFile (fromString (dir </> "file"))
  missing <- statTaskFile (fromString (dir </> "missing"))
  directory <- try @IOException $ statTaskFile (fromString dir)
  writeFile (dir </> "empty") ""
  empty <- statTaskFile (fromString (dir </> "empty"))
  underFile <- try @IOException $ statTaskFile (fromString (dir </> "file" </> "child"))
  createFileLink (dir </> "file") (dir </> "link")
  createFileLink (dir </> "nowhere") (dir </> "dangling")
  link <- statTaskFile (fromString (dir </> "link"))
  dangling <- statTaskFile (fromString (dir </> "dangling"))
  let
    file = fromString (dir </> "file")
    other = fromString (dir </> "other")
  writeFile (dir </> "other") "abc"
  sizes <- runTaskFilesWith (== other) $ do
    first <- taskFileSize file
    -- Removed behind its back: the first answer stands.
    liftIO $ removeFile (dir </> "file")
    second <- taskFileSize file
    absent <- taskFileSize other
    pure (first, second, absent)
  removeDirectoryRecursive dir
  expect "a regular file has its size" $ fileSize == Just 5000
  expect "a missing path has no size" $ missing == Nothing
  expect "a directory is an error" $ isLeft directory
  expect "an empty file has size 0" $ empty == Just 0
  -- ENOTDIR is not "does not exist" for GHC: an error, as with getFileStatus before
  expect "a path under a regular file is an error" $ isLeft underFile
  expect "a symbolic link has its target's size" $ link == Just 5000
  expect "a dangling symbolic link has no size" $ dangling == Nothing
  expect "a path is stat'ed once, a path known to be absent never" $
    sizes == (Just 5000, Just 5000, Nothing)
  putStrLn "All TaskFiles tests passed."
  where
    removeIfPresent d = () <$ try @IOException (removeDirectoryRecursive d)
