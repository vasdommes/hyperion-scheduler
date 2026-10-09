{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

-- | How often one run of a 'ComputeValueTask' evaluates its computation and
-- reads its inputs. Runs a local 'Process' on files in a temporary directory.
module Hyperion.Scheduler.Test.ComputeValueTest where

import Bootstrap.Build                 (Fetches (..))
import Control.Exception               (AssertionFailed (..), IOException,
                                        throwIO, try)
import Control.Monad                   (unless)
import Data.Aeson                      (ToJSON)
import Data.Binary                     (Binary)
import Data.Binary                     qualified as Binary
import Data.IORef                      (IORef, atomicModifyIORef', readIORef)
import Hyperion                        (Dict (..), defaultHostNameStrategy,
                                        runProcessLocal)
import Hyperion.OsString               (fromString)
import Hyperion.OsString               qualified as OsString
import Hyperion.Scheduler.PathResolver (PathResolver (..))
import Hyperion.Scheduler.StatKey      (ToFileStatKey)
import Hyperion.Scheduler.Task.Task    (ComputeValue (..), DepKeys, Task (..),
                                        TaskKey, ValueSerializable (..),
                                        ValueType, computeAndWrite)
import Hyperion.Scheduler.Test.Counted (counted, newCounter, resetCounter)
import System.Directory                (createDirectory, getTemporaryDirectory,
                                        removeDirectoryRecursive)
import System.FilePath                 ((</>))
import System.IO.Unsafe                (unsafePerformIO)

-- | An input file holding @10 * i@.
newtype InKey = MkInKey Int
  deriving newtype (Eq, Ord, Show)

type instance ValueType InKey = Int

instance ToFileStatKey InKey

instance ValueSerializable InKey where
  readValue _ path = do
    atomicModifyIORef' inputReads (\n -> (n + 1, ()))
    Binary.decodeFile (OsString.toString path)

-- | The sum of inputs @1..n@, each fetched twice.
newtype SumKey = MkSumKey Int
  deriving newtype (Eq, Ord, Show, Binary, ToJSON)

type instance ValueType SumKey = Int
type instance DepKeys SumKey = '[InKey]

instance ToFileStatKey SumKey

instance ValueSerializable SumKey

instance TaskKey SumKey

instance ComputeValue SumKey where
  computeValue _ _ key@(MkSumKey n) = counted computations key $
    sum <$> traverse (fetchIn . MkInKey) ([1 .. n] <> [1 .. n])
    where
      fetchIn :: Fetches InKey Int f => InKey -> f Int
      fetchIn = fetch

newtype DirResolver = MkDirResolver FilePath

instance PathResolver DirResolver InKey where
  resolvePath (MkDirResolver dir) (MkInKey i) =
    fromString (dir </> ("in-" <> show i))

instance PathResolver DirResolver SumKey where
  resolvePath (MkDirResolver dir) (MkSumKey n) =
    fromString (dir </> ("sum-" <> show n))

computations :: IORef Int
computations = unsafePerformIO newCounter
{-# NOINLINE computations #-}

inputReads :: IORef Int
inputReads = unsafePerformIO newCounter
{-# NOINLINE inputReads #-}

expect :: String -> Bool -> IO ()
expect label cond = do
  unless cond $ throwIO $ AssertionFailed $ "FAILED: " <> label
  putStrLn $ "ok: " <> label

runTest :: IO ()
runTest = do
  tmp <- getTemporaryDirectory
  let
    dir = tmp </> "hyperion-scheduler-compute-value-test"
    resolver = MkDirResolver dir
    key = MkSumKey 3
  () <$ try @IOException (removeDirectoryRecursive dir)
  createDirectory dir
  mapM_
    (\i -> Binary.encodeFile
      (OsString.toString (resolvePath resolver (MkInKey i))) (10 * i :: Int))
    [1 .. 3]
  resetCounter computations
  resetCounter inputReads
  runProcessLocal defaultHostNameStrategy $
    computeAndWrite Dict 1 (MkTask resolver () key)
  value <- Binary.decodeFile @Int (OsString.toString (resolvePath resolver key))
  numComputations <- readIORef computations
  numReads <- readIORef inputReads
  removeDirectoryRecursive dir
  expect "the value is the sum of the inputs, each fetched twice" $
    value == 120
  expect "one run evaluates the computation twice" $ numComputations == 2
  expect "one run reads each input once" $ numReads == 3
  putStrLn "All ComputeValue tests passed."
