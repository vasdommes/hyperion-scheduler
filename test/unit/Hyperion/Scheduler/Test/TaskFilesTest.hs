-- | What 'statTaskFile' reports for a regular file, a missing path and a
-- directory, and what 'runTaskFilesWith' remembers and treats as absent.
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
import System.Directory             (createDirectory, getTemporaryDirectory,
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
  expect "a path is stat'ed once, a path known to be absent never" $
    sizes == (Just 5000, Just 5000, Nothing)
  putStrLn "All TaskFiles tests passed."
  where
    removeIfPresent d = () <$ try @IOException (removeDirectoryRecursive d)
