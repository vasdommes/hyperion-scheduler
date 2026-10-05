-- | What 'statTaskFile' reports for a regular file, a missing path and a
-- directory. Uses a temporary directory.
module Hyperion.Scheduler.Test.TaskFilesTest where

import Control.Exception            (AssertionFailed (..), IOException, throwIO,
                                     try)
import Control.Monad                (unless)
import Data.Either                  (isLeft)
import Hyperion.OsString            (fromString)
import Hyperion.Scheduler.TaskFiles (statTaskFile)
import System.Directory             (createDirectory, getTemporaryDirectory,
                                     removeDirectoryRecursive)
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
  removeDirectoryRecursive dir
  expect "a regular file has its size" $ fileSize == Just 5000
  expect "a missing path has no size" $ missing == Nothing
  expect "a directory is an error" $ isLeft directory
  putStrLn "All TaskFiles tests passed."
  where
    removeIfPresent d = () <$ try @IOException (removeDirectoryRecursive d)
