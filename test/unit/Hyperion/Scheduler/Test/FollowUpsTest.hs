{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StaticPointers        #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Unit test of follow-ups declared by a key ('FollowUps'): the task chain
-- gives a task whose key declares follow-ups a builder that makes the
-- follow-up's task map with the same resolver and configs; a key that may
-- add a task of its own kind makes the instance recursive. Pure: builds
-- task maps under a directory that does not exist, so nothing is pruned.
module Hyperion.Scheduler.Test.FollowUpsTest (runTest) where

import Bootstrap.Build                     (Variant (..))
import Control.Exception                   (AssertionFailed (..), throwIO)
import Control.Monad                       (unless)
import Data.Aeson                          (ToJSON)
import Data.Binary                         (Binary)
import Data.Functor                        (($>))
import Data.Map.Strict                     qualified as Map
import Data.Maybe                          (isJust, isNothing)
import Data.Set                            qualified as Set
import GHC.Generics                        (Generic)
import Hyperion                            (Dict (..), Static (..))
import Hyperion.OsPath                     (OsPath, (<.>), (</>))
import Hyperion.OsString                   (fromString, showOs)
import Hyperion.Scheduler.FilePath         (VirtualFilePath (..))
import Hyperion.Scheduler.PathResolver     (PathResolver (..))
import Hyperion.Scheduler.StatKey          (TaskKeyFileInfo (..),
                                            ToFileStatKey (..),
                                            mkFileStatKeyViaJSON)
import Hyperion.Scheduler.Task.FollowUps   (encodeFollowUp)
import Hyperion.Scheduler.Task.IsTask      (IsTask (..))
import Hyperion.Scheduler.Task.Task        (DepKeys, TaskKey (..),
                                            TaskKind (..), getPath)
import Hyperion.Scheduler.Task.TaskMap     (TaskMap, mkTaskMap)
import Hyperion.Scheduler.Task.WrappedTask (WrappedTask)

-- * Keys: a search in rounds. A decision may add the next decision or the
-- final task; a final task adds nothing.

newtype Decide = MkDecide Int
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Binary, ToJSON)

newtype Final = MkFinal Int
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Binary, ToJSON)

type instance DepKeys Decide = '[]
type instance DepKeys Final = '[]

instance ToFileStatKey Decide where toFileStatKey = mkFileStatKeyViaJSON
instance ToFileStatKey Final where toFileStatKey = mkFileStatKeyViaJSON

instance TaskKey Decide where
  type FollowUps Decide = '[Decide, Final]
  taskKind = CustomTaskWithHandle $ \_ _ _ key -> getPath key $> pure ()

instance TaskKey Final where
  taskKind = CustomTask $ \_ _ key -> getPath key $> pure ()

instance Static (Binary Decide) where closureDict = static Dict
instance Static (Binary Final) where closureDict = static Dict
instance Static (TaskKey Decide) where closureDict = static Dict
instance Static (TaskKey Final) where closureDict = static Dict

-- * A resolver: one directory.

newtype Dir = MkDir OsPath
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Binary, ToJSON)

instance PathResolver Dir Decide where
  resolvePath (MkDir dir) (MkDecide r) = dir </> "decide_" <> showOs r <.> "txt"

instance PathResolver Dir Final where
  resolvePath (MkDir dir) (MkFinal r) = dir </> "final_" <> showOs r <.> "txt"

instance Static (Binary Dir) where closureDict = static Dict
instance Static (PathResolver Dir Decide) where closureDict = static Dict
instance Static (PathResolver Dir Final) where closureDict = static Dict

-- * The test

expect :: String -> Bool -> IO ()
expect label cond = do
  unless cond $ throwIO $ AssertionFailed $ "FAILED: " <> label
  putStrLn $ "ok: " <> label

outputPaths :: TaskMap WrappedTask -> [OsPath]
outputPaths taskMap =
  [ path | task <- Map.keys taskMap, MkTaskKeyFileInfo { path = VirtualFilePath path } <- Set.toList (taskOutputs task) ]

testDir :: Dir
testDir = MkDir (fromString "/nonexistent/followups-test")

runTest :: IO ()
runTest = do
  decideMap <- mkTaskMap testDir () (MkDecide 0)
  expect "the decision's map has the decision alone" $ Map.size decideMap == 1
  let decideTask = head (Map.keys decideMap)
  expect "a key with follow-ups gets a builder" $ isJust (taskFollowUps decideTask)
  case taskFollowUps decideTask of
    Nothing -> pure ()
    Just build -> do
      nextRound <- build (encodeFollowUp (VLeft (MkDecide 1) :: Variant (FollowUps Decide)))
      expect "the next decision is built with the same resolver" $
        outputPaths nextRound == [resolvePath testDir (MkDecide 1)]
      final <- build (encodeFollowUp (VRight (VLeft (MkFinal 1)) :: Variant (FollowUps Decide)))
      expect "the final task is built with the same resolver" $
        outputPaths final == [resolvePath testDir (MkFinal 1)]
      -- The follow-up's own follow-ups: the next decision may add again.
      let nextTask = head (Map.keys nextRound)
      expect "a follow-up decision has a builder of its own" $ isJust (taskFollowUps nextTask)
      expect "the final task declares no follow-ups" $
        isNothing (taskFollowUps (head (Map.keys final)))
  putStrLn "All follow-up tests passed."
