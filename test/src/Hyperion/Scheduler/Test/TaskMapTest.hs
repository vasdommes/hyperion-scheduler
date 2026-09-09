{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Pure unit tests for TaskMap logic: validateTaskMap, replaceTasks and
-- placeholder tasks. Runs locally, no cluster or filesystem access needed.
module Hyperion.Scheduler.Test.TaskMapTest where

import Control.Exception               (AssertionFailed (..), throwIO)
import Control.Monad                   (unless)
import Data.Aeson                      (ToJSON)
import Data.Binary                     (Binary)
import Data.Map.Strict                 qualified as Map
import Data.Maybe                      (isNothing)
import Data.Set                        (Set)
import Data.Set                        qualified as Set
import Data.Text                       qualified as Text
import GHC.Generics                    (Generic)
import Hyperion.OsPath                 (OsPath)
import Hyperion.OsString               (fromString)
import Hyperion.Scheduler.FilePath     (VirtualFilePath (..))
import Hyperion.Scheduler.StatKey      (TaskKeyFileInfo (..),
                                        ToFileStatKey (..),
                                        mkFileStatKeyViaJSON)
import Hyperion.Scheduler.Task.IsTask  (IsTask (..), taskInputPaths)
import Hyperion.Scheduler.Task.Task    (DepKeys, ListTaskKey (..), TaskKey (..),
                                        TaskKind (..), dependencies, outKeys)
import Hyperion.Scheduler.Task.TaskMap (TaskMap, placeholdersOfType,
                                        replaceTasks, validateTaskMap)

-- * A minimal IsTask for building TaskMaps by hand

data TestTask = MkTestTask
  { name          :: String
  , inputs        :: Set OsPath
  , outputs       :: Set OsPath
  , isPlaceholder :: Bool
  } deriving (Eq, Ord, Show, Generic, ToJSON)

mkFileInfo :: OsPath -> TaskKeyFileInfo
mkFileInfo path = MkTaskKeyFileInfo
  { fileStatKey = mkFileStatKeyViaJSON (show path)
  , path        = VirtualFilePath path
  , fileSize    = 0
  }

instance IsTask TestTask where
  taskInputs t        = Set.map mkFileInfo t.inputs
  taskOutputs t       = Set.map mkFileInfo t.outputs
  taskTag t           = Just (Text.pack t.name)
  taskClosure _ _     = Nothing
  taskIsPlaceholder t = t.isPlaceholder

testTask :: String -> [OsPath] -> [OsPath] -> TestTask
testTask name ins outs = MkTestTask
  { name          = name
  , inputs        = Set.fromList ins
  , outputs       = Set.fromList outs
  , isPlaceholder = False
  }

-- * A placeholder TaskKey, exercising the real 'TaskKind' machinery

newtype PKey = MkPKey String
  deriving newtype (Eq, Ord, Show, Binary, ToJSON)

type instance DepKeys PKey = '[]

instance ToFileStatKey PKey where
  toFileStatKey = mkFileStatKeyViaJSON

instance TaskKey PKey where
  taskKind = PlaceholderTask

-- * Assertions

expect :: String -> Bool -> IO ()
expect label cond = do
  unless cond $ throwIO $ AssertionFailed $ "FAILED: " <> label
  putStrLn $ "ok: " <> label

expectValid :: String -> TaskMap TestTask -> IO ()
expectValid label taskMap = case validateTaskMap taskMap of
  Right () -> putStrLn $ "ok: " <> label
  Left err -> throwIO $ AssertionFailed $ "FAILED: " <> label <> ": unexpectedly invalid: " <> show err

expectInvalid :: String -> TaskMap TestTask -> IO ()
expectInvalid label taskMap = case validateTaskMap taskMap of
  Left _   -> putStrLn $ "ok: " <> label
  Right () -> throwIO $ AssertionFailed $ "FAILED: " <> label <> ": unexpectedly valid"

-- * Tests

p1, p2, q, r :: OsPath
p1 = fromString "/data/p1"
p2 = fromString "/data/p2"
q  = fromString "/data/q"
r  = fromString "/data/r"

-- | An input path produced by NO task in the map is assumed to already exist
-- on disk (its producer was pruned by checkCreated): valid.
testPrunedInputIsValid :: IO ()
testPrunedInputIsValid = do
  let
    a = testTask "A" [p1] [q]
    taskMap = Map.fromList [(a, Set.empty)]
  expectValid "input with no producer in map (pruned dependency) is valid" taskMap

-- | An input path produced by a task IN the map, with no dependency edge to
-- it: invalid (this is the broken-graph case).
testMissingEdgeIsInvalid :: IO ()
testMissingEdgeIsInvalid = do
  let
    a = testTask "A" [p1] [q]
    b = testTask "B" [] [p1]
    taskMap = Map.fromList [(a, Set.empty), (b, Set.empty)]
  expectInvalid "input produced in map but not by dependencies is invalid" taskMap

-- | The same graph with the edge present: valid.
testCorrectEdgeIsValid :: IO ()
testCorrectEdgeIsValid = do
  let
    a = testTask "A" [p1] [q]
    b = testTask "B" [] [p1]
    taskMap = Map.fromList [(a, Set.singleton b), (b, Set.empty)]
  expectValid "input produced by a dependency is valid" taskMap

-- | Unreplaced placeholder tasks are rejected.
testUnreplacedPlaceholderIsInvalid :: IO ()
testUnreplacedPlaceholderIsInvalid = do
  let
    d = (testTask "D" [] [p1]) { isPlaceholder = True }
    a = testTask "A" [p1] [q]
    taskMap = Map.fromList [(a, Set.singleton d), (d, Set.empty)]
  expectInvalid "unreplaced placeholder is invalid" taskMap

-- | replaceTasks connects dependents only to the roots of the replacement
-- fragment, not to every task in it.
testReplaceTasksConnectsToRootsOnly :: IO ()
testReplaceTasksConnectsToRootsOnly = do
  let
    d = (testTask "D" [] [p1]) { isPlaceholder = True }   -- placeholder for W
    a = testTask "A" [p1] [q]                             -- consumer
    w = testTask "W" [r] [p1]                             -- fragment root
    x = testTask "X" [] [r]                               -- fragment dependency
    taskMap  = Map.fromList [(a, Set.singleton d), (d, Set.empty)]
    fragment = Map.fromList [(w, Set.singleton x), (x, Set.empty)]
    replaced = replaceTasks (Map.singleton d fragment) taskMap
  expect "placeholder removed from keys" $ not (Map.member d replaced)
  expect "consumer depends exactly on the fragment root" $
    Map.lookup a replaced == Just (Set.singleton w)
  expect "fragment edges preserved" $
    Map.lookup w replaced == Just (Set.singleton x) && Map.lookup x replaced == Just Set.empty
  expectValid "replaced map is valid" replaced

-- | PlaceholderTask semantics at the TaskKey level: output is definitionally
-- the key itself, no dependencies, nothing to forget.
testPlaceholderTaskKeySemantics :: IO ()
testPlaceholderTaskKeySemantics = do
  let key = MkPKey "some-block"
  expect "placeholder outKeys is the key itself" $
    outKeys () key == Set.singleton key
  expect "placeholder has no dependencies" $
    Set.null (dependencies () key)

-- | NoOpTask semantics via ListTaskKey: no outputs, one dependency edge per
-- element.
testNoOpTaskKeySemantics :: IO ()
testNoOpTaskKeySemantics = do
  let listKey = MkListTaskKey [MkPKey "a", MkPKey "b"]
  expect "list task has no outputs" $
    Set.null (outKeys () listKey)
  expect "list task depends on all its elements" $
    Set.size (dependencies () listKey) == 2

-- | placeholdersOfType finds placeholders via taskPlaceholderKey.
testPlaceholdersOfType :: IO ()
testPlaceholdersOfType = do
  let
    a = testTask "A" [p1] [q]
    taskMap = Map.fromList [(a, Set.empty)]
  -- TestTask never implements taskPlaceholderKey, so nothing is found; the
  -- real Task r k implementation is exercised in the blocks-3d tests.
  expect "placeholdersOfType finds nothing among non-placeholders" $
    null (placeholdersOfType @TestTask @PKey taskMap)
  expect "taskPlaceholderKey defaults to Nothing" $
    isNothing (taskPlaceholderKey @TestTask @PKey a)
  expect "taskInputPaths of test task" $
    taskInputPaths a == Set.singleton (VirtualFilePath p1)

runTest :: IO ()
runTest = do
  testPrunedInputIsValid
  testMissingEdgeIsInvalid
  testCorrectEdgeIsValid
  testUnreplacedPlaceholderIsInvalid
  testReplaceTasksConnectsToRootsOnly
  testPlaceholderTaskKeySemantics
  testNoOpTaskKeySemantics
  testPlaceholdersOfType
  putStrLn "All TaskMap tests passed."
