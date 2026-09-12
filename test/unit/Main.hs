-- | Pure unit tests. No cluster, no filesystem, no SLURM: @stack test@.
module Main where

import Hyperion.Scheduler.Test.TaskMapTest qualified as TaskMapTest

main :: IO ()
main = TaskMapTest.runTest
