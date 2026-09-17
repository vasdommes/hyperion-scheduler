-- | Pure unit tests. No cluster, no filesystem, no SLURM: @stack test@.
module Main where

import Hyperion.Scheduler.Test.FollowUpsTest qualified as FollowUpsTest
import Hyperion.Scheduler.Test.MemoryTest qualified as MemoryTest
import Hyperion.Scheduler.Test.TaskMapTest qualified as TaskMapTest

main :: IO ()
main = do
  TaskMapTest.runTest
  FollowUpsTest.runTest
  MemoryTest.runTest
