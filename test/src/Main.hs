module Main where

import Hyperion.Scheduler.Test.LinearTransform qualified as LinearTransform

main :: IO ()
main = do
  LinearTransform.runTest
