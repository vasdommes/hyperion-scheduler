module Main where

import Hyperion.Scheduler.Test.LinearTransform qualified as LinearTransform
import Hyperion.Scheduler.Test.TaskMapTest     qualified as TaskMapTest

main :: IO ()
main = do
  -- Pure unit tests (no cluster needed)
  TaskMapTest.runTest
  -- Cluster test
  LinearTransform.runTest
