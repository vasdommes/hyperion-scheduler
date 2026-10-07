-- | Unit tests. No cluster, no SLURM: @stack test@. Only 'TaskFilesTest'
-- touches the filesystem, in a temporary directory.
module Main where

import Hyperion.Scheduler.Test.EstimateTest     qualified as EstimateTest
import Hyperion.Scheduler.Test.InputSummaryTest qualified as InputSummaryTest
import Hyperion.Scheduler.Test.TaskFilesTest    qualified as TaskFilesTest
import Hyperion.Scheduler.Test.TaskMapTest      qualified as TaskMapTest

main :: IO ()
main = do
  TaskMapTest.runTest
  EstimateTest.runTest
  InputSummaryTest.runTest
  TaskFilesTest.runTest
