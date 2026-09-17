-- | Unit test of the @VmHWM@ parser behind the task-memory measurement
-- ('Hyperion.Scheduler.RunTasks.RemoteRunTask.afterReturnMemoryM').
module Hyperion.Scheduler.Test.MemoryTest (runTest) where

import Control.Exception                        (AssertionFailed (..), throwIO)
import Control.Monad                            (unless)
import Hyperion.Scheduler.RunTasks.RemoteRunTask (parseVmHWM)
import Hyperion.Scheduler.Types                 (MemorySize (..))

-- | The shape of @/proc/<pid>/status@ on Linux, abridged.
sampleStatus :: String
sampleStatus = unlines
  [ "Name:\thyperion"
  , "VmPeak:\t 1234567 kB"
  , "VmSize:\t 1000000 kB"
  , "VmHWM:\t    9728 kB"
  , "VmRSS:\t    9000 kB"
  , "Threads:\t4"
  ]

runTest :: IO ()
runTest = do
  check "the VmHWM line, in kB, becomes bytes"
    (parseVmHWM sampleStatus == Just (MemorySize (9728 * 1024)))
  check "a text without a VmHWM line gives Nothing"
    (parseVmHWM "Name:\thyperion\nVmRSS:\t 9000 kB\n" == Nothing)
  check "a malformed number gives Nothing"
    (parseVmHWM "VmHWM:\t abc kB\n" == Nothing)
  check "an empty text gives Nothing"
    (parseVmHWM "" == Nothing)
  putStrLn "MemoryTest: 4 checks passed"
  where
    check name ok = unless ok $ throwIO (AssertionFailed ("MemoryTest: " <> name))
