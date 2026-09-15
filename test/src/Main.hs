module Main where

import Hyperion.Scheduler.Test.FollowUps       qualified as FollowUps
import Hyperion.Scheduler.Test.LinearTransform qualified as LinearTransform
import System.Environment                      (lookupEnv)

-- | Select the test with HYPERION_SCHEDULER_TEST=linear (default) or
-- =followups.
main :: IO ()
main = do
  which <- lookupEnv "HYPERION_SCHEDULER_TEST"
  case which of
    Just "followups" -> FollowUps.runTest
    _             -> LinearTransform.runTest
