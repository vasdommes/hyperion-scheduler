{-# LANGUAGE OverloadedRecordDot #-}

-- | Pure unit tests for statistics. Runs locally, no cluster or filesystem
-- access needed.
module Hyperion.Scheduler.Test.EstimateTest where

import Control.Exception        (AssertionFailed (..), throwIO)
import Control.Monad            (unless)
import Data.List.NonEmpty       qualified as NonEmpty
import Data.Text                qualified as Text
import Hyperion.Scheduler.Stats (Trials (..), toTrials)
import Hyperion.Scheduler.Util  (qualifiedTypeRepText)

expect :: String -> Bool -> IO ()
expect label cond = do
  unless cond $ throwIO $ AssertionFailed $ "FAILED: " <> label
  putStrLn $ "ok: " <> label

-- | Merging is the only arithmetic these statistics do, and it had no test.
-- Checked against the mean and biased variance computed directly from the same
-- observations.
testTrialsSummary :: IO ()
testTrialsSummary = do
  let
    xs = [1, 2, 3, 4, 10] :: [Double]
    trials = toTrials (NonEmpty.fromList xs)
    reversed = toTrials (NonEmpty.fromList (reverse xs))
    n = fromIntegral (length xs) :: Double
    expectedMean = sum xs / n
    expectedVariance =
      sum (map (^ (2 :: Int)) xs) / n - expectedMean * expectedMean
    close a b = abs (a - b) < 1e-9
  expect "mean of the observations" $ close trials.mean expectedMean
  expect "biased variance of the observations" $
    close trials.variance expectedVariance
  expect "extremes and count of the observations" $
    (trials.min, trials.max, trials.numTrials) == (1, 10, 5)
  expect "the summary does not depend on merge order" $
    close reversed.mean trials.mean && close reversed.variance trials.variance

-- | Stat key types are told apart by this name, so that types of the same
-- name in different modules do not share statistics.
data TaggedKey = MkTaggedKey

testQualifiedTypeName :: IO ()
testQualifiedTypeName =
  expect "a type's qualified name has its module" $
    Text.unpack (qualifiedTypeRepText @TaggedKey)
      == "Hyperion.Scheduler.Test.EstimateTest.TaggedKey"

runTest :: IO ()
runTest = do
  testTrialsSummary
  testQualifiedTypeName
  putStrLn "All estimate tests passed."
