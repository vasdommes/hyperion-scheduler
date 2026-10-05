{-# LANGUAGE OverloadedStrings #-}

-- | Pure unit tests for summaries of a task's input files. No cluster or
-- filesystem access needed.
module Hyperion.Scheduler.Test.InputSummaryTest where

import Control.Exception          (AssertionFailed (..), throwIO)
import Control.Monad              (unless)
import Data.Aeson                 qualified as Aeson
import Hyperion.Scheduler.StatKey (FileStatKey (..), FromInputFiles (..),
                                   InputFileSizes (..), InputFileSummary (..),
                                   KeyedInputFileSizes (..),
                                   MaxInputFileSize (..),
                                   TotalInputFileSize (..))

expect :: String -> Bool -> IO ()
expect label cond = do
  unless cond $ throwIO $ AssertionFailed $ "FAILED: " <> label
  putStrLn $ "ok: " <> label

-- | The stock summaries, built one file at a time.
testStockSummaries :: IO ()
testStockSummaries = do
  let
    keyA = MkFileStatKey (Aeson.String "a")
    keyB = MkFileStatKey (Aeson.String "b")
    files =
      [ MkInputFileSummary { fileStatKey = Nothing,   size = 30 }
      , MkInputFileSummary { fileStatKey = Just keyB, size = 10 }
      , MkInputFileSummary { fileStatKey = Just keyA, size = 20 }
      ]
    summarize :: FromInputFiles s => [InputFileSummary] -> s
    summarize = foldMap fromInputFile
  expect "the size of the largest file" $
    summarize files == MkMaxInputFileSize 30
  expect "the total size" $
    summarize files == MkTotalInputFileSize 60
  expect "the sizes, sorted" $
    summarize files == MkInputFileSizes [10, 20, 30]
  expect "the keys and sizes, sorted" $
    summarize files == MkKeyedInputFileSizes [(Nothing, 30), (Just keyA, 20), (Just keyB, 10)]
  expect "two summaries of the same files" $
    summarize files == (MkMaxInputFileSize 30, MkTotalInputFileSize 60)
  expect "no files" $
    summarize [] == (MkMaxInputFileSize 0, MkTotalInputFileSize 0)

runTest :: IO ()
runTest = do
  testStockSummaries
  putStrLn "All InputSummary tests passed."
