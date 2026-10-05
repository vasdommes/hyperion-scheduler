{-# LANGUAGE OverloadedRecordDot #-}

-- | Pure unit tests for statistics. Runs locally, no cluster or filesystem
-- access needed.
module Hyperion.Scheduler.Test.EstimateTest where

import Control.Exception        (AssertionFailed (..), throwIO)
import Control.Monad            (unless)
import Data.List.NonEmpty       qualified as NonEmpty
import Hyperion.Scheduler.Stats (Trials (..), toTrials)

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
    expectedVariance = sum (map (^ (2 :: Int)) xs) / n - expectedMean * expectedMean
    close a b = abs (a - b) < 1e-9
  expect "mean of the observations" $ close trials.mean expectedMean
  expect "biased variance of the observations" $ close trials.variance expectedVariance
  expect "extremes and count of the observations" $
    (trials.min, trials.max, trials.numTrials) == (1, 10, 5)
  expect "the summary does not depend on merge order" $
    close reversed.mean trials.mean && close reversed.variance trials.variance

runTest :: IO ()
runTest = do
  testTrialsSummary
  putStrLn "All estimate tests passed."
