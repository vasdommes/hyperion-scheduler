{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Pure unit tests for estimates that depend on the sizes of a task's input
-- files ('summarizeTask'). No cluster or filesystem access needed.
module Hyperion.Scheduler.Test.InputSummaryTest where

import Control.Exception               (AssertionFailed (..), throwIO)
import Control.Monad                   (unless)
import Data.Aeson                      (FromJSON, ToJSON)
import Data.Aeson                      qualified as Aeson
import Data.Binary                     (Binary)
import Data.Foldable                   (traverse_)
import Data.Map.Strict                 qualified as Map
import Data.Set                        qualified as Set
import Hyperion.OsPath                 (OsPath)
import Hyperion.OsString               (fromString)
import Hyperion.Scheduler.FilePath     (VirtualFilePath (..))
import Hyperion.Scheduler.PathResolver (PathResolver (..))
import Hyperion.Scheduler.StatKey      (FileStatKey (..), FromInputFiles (..),
                                        InputFileSizes (..),
                                        InputFileSummary (..),
                                        IsFileStatKey (..), IsStatKey (..),
                                        KeyedInputFileSizes (..),
                                        MaxInputFileSize (..),
                                        TaskKeyFileInfo (..),
                                        ToFileStatKey (..),
                                        TotalInputFileSize (..))
import Hyperion.Scheduler.Task.IsTask  (ResourceEstimates (..),
                                        TaskSummary (..))
import Hyperion.Scheduler.Task.Task    (DepKeys, Task (..), TaskKey (..),
                                        TaskKind (..), getPath, summarizeTask)
import Hyperion.Scheduler.Types        (Estimate (..))

-- | An input file, produced elsewhere.
newtype LeafKey = MkLeafKey Int
  deriving newtype (Eq, Ord, Show, Binary, ToJSON)

type instance DepKeys LeafKey = '[]

instance TaskKey LeafKey where
  taskKind = PlaceholderTask

instance ToFileStatKey LeafKey

-- | Sums the given leaves.
newtype SumKey = MkSumKey [Int]
  deriving newtype (Eq, Ord, Show, Binary, ToJSON)

type instance DepKeys SumKey = '[LeafKey]

data SumStatKey = MkSumStatKey
  deriving (Eq, Ord, Show)

instance ToJSON SumStatKey where
  toJSON _ = Aeson.Null

instance FromJSON SumStatKey where
  parseJSON _ = pure MkSumStatKey

-- | Memory is twice the total input.
instance IsStatKey SumStatKey where
  type InputSummary SumStatKey = TotalInputFileSize
  memoryEstimate _ (MkTotalInputFileSize total) = 2 * fromIntegral total

-- | The output is one byte more than the largest input.
instance IsFileStatKey SumStatKey where
  type ProducerInputSummary SumStatKey = MaxInputFileSize
  fileSizeEstimate _ (MkMaxInputFileSize largest) = largest + 1

instance ToFileStatKey SumKey where
  type FileStatKeyOf SumKey = SumStatKey
  fileStatKeyOf _ = Just MkSumStatKey

instance TaskKey SumKey where
  type StatKeyOf SumKey = SumStatKey
  toStatKey _ _ = Just MkSumStatKey
  taskKind = CustomTask $ \_ _ key@(MkSumKey leaves) ->
    getPath key *> traverse_ (getPath . MkLeafKey) leaves *> pure (pure ())

data TestResolver = MkTestResolver

instance PathResolver TestResolver LeafKey where
  resolvePath _ (MkLeafKey i) = fromString ("/leaf/" <> show i)

instance PathResolver TestResolver SumKey where
  resolvePath _ _ = sumPath

sumPath :: OsPath
sumPath = fromString "/sum"

leafPath :: Int -> VirtualFilePath
leafPath i = VirtualFilePath (fromString ("/leaf/" <> show i))

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

testSummarizeTask :: IO ()
testSummarizeTask = do
  let
    known path = Map.lookup path $ Map.fromList
      [ (leafPath 1, info 1 (EstimatedByTask 10))
      , (leafPath 2, info 2 (MeasuredFromStats 30 20))
      ]
    info i size = MkTaskKeyFileInfo { path = leafPath i, fileStatKey = Nothing, fileSize = size }
    summary = summarizeTask (Just known) MkTask
      { resolver = MkTestResolver, config = (), key = MkSumKey [1, 2, 3] }
    outputSizes = [ (o.path, o.fileSize) | o <- Set.toList summary.outputs ]
  expect "inputs take the known infos, an unknown one the size 0" $
    [ (i.path, i.fileSize) | i <- Set.toList summary.inputs ] ==
      [ (leafPath 1, EstimatedByTask 10)
      , (leafPath 2, MeasuredFromStats 30 20)
      , (leafPath 3, EstimatedByTask 0)
      ]
  expect "memory is the model of the input summary, from scheduled sizes" $
    summary.estimates.memory == EstimatedByTask 80
  expect "an output size is the model of the producer input summary" $
    outputSizes == [(VirtualFilePath sumPath, EstimatedByTask 31)]

runTest :: IO ()
runTest = do
  testStockSummaries
  testSummarizeTask
  putStrLn "All InputSummary tests passed."
