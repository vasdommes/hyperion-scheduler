{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StaticPointers        #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Test of follow-ups declared by a key ('FollowUps', 'addFollowUp'): the
-- search of "Hyperion.Scheduler.Test.FollowUps" written with 'TaskKey' keys
-- and 'mkTaskMap', so that the tasks are built from a resolver and configs
-- the way a real build's are. The decision task names the next round's
-- decision, or the final task, and the scheduler builds its map with the
-- resolver and configs the decision was built with; no task body holds a
-- resolver, and the resolver type appears only where the run is set up.
--
-- Round @r@ has one @Block@ per block, which halves the block's value (round
-- 0 starts from a fixed value), and one @Decide@ that reads every block's
-- value and adds round @r+1@ or the @Final@ sum. The initial map is the
-- round-0 decision, which reaches the round-0 blocks through 'mkTaskMap'.
-- Checks: the final round and sum against a direct computation, the number
-- of task records, and, with node-local block values, no leftover files.
module Hyperion.Scheduler.Test.FollowUpKeys where

import Bootstrap.Build                 (Fetches (..), Variant (..))
import Control.Exception               (AssertionFailed (..))
import Control.Monad                   (filterM, unless)
import Control.Monad.IO.Class          (liftIO)
import Data.Aeson                      (ToJSON)
import Data.Binary                     (Binary)
import GHC.Generics                    (Generic)
import Hyperion                        hiding (NumCPUs)
import Hyperion.Log                    qualified as Log
import Hyperion.OsPath                 (OsPath, (<.>), (</>))
import Hyperion.OsString               (showOs)
import Hyperion.Scheduler              (ComputeValue (..), DepKeys, FetchesPath,
                                        NumCPUs, PathResolver (..), TaskHandle,
                                        TaskKey (..), TaskKind (..),
                                        ToFileStatKey (..),
                                        ValueSerializable (..), ValueType,
                                        addFollowUp, getPath, mkFileStatKeyViaJSON,
                                        mkTaskMap, recordToTaskStats, runTasks,
                                        unWrappedProcess, writeTaskStats)
import Hyperion.Scheduler.Config       qualified as Scheduler
import Hyperion.Scheduler.Task.HasConfig (Configs (..))
import System.Directory.OsPath         (createDirectoryIfMissing,
                                        doesFileExist, removePathForcibly)

---------- The search ----------

-- | The shape of the search: the config of every task.
data Search = MkSearch
  { numBlocks :: Int
  , threshold :: Int
  } deriving (Eq, Ord, Show, Generic, Binary, ToJSON)

instance Static (Binary Search) where closureDict = static Dict

initialValue :: Int -> Int
initialValue i = 1000 + 37 * i

valueAt :: Int -> Int -> Int
valueAt r i = iterate (`div` 2) (initialValue i) !! r

expectedResult :: Search -> (Int, Int)
expectedResult search = go 0
  where
    values r = map (valueAt r) [1 .. search.numBlocks]
    go r
      | maximum (values r) < search.threshold = (r, sum (values r))
      | otherwise                             = go (r + 1)

-- | Blocks, decisions and the final task of a search that stops at the
-- given round.
expectedTaskCount :: Search -> Int -> Int
expectedTaskCount search finalRound = (finalRound + 1) * search.numBlocks + (finalRound + 1) + 1

---------- Keys ----------

data BlockKey = MkBlockKey { round :: Int, index :: Int }
  deriving (Eq, Ord, Show, Generic, Binary, ToJSON)

-- | The block of the previous round, as a dependency of the next round's
-- block: a type of its own so that a block's body cannot confuse its output
-- with its input (see the note on same-type fetches in the bfss plan).
newtype PriorBlockKey = MkPriorBlockKey BlockKey
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Binary, ToJSON)

newtype DecideKey = MkDecideKey { round :: Int }
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Binary, ToJSON)

newtype FinalKey = MkFinalKey { round :: Int }
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Binary, ToJSON)

type instance ValueType BlockKey      = Int
type instance ValueType PriorBlockKey = Int
type instance ValueType DecideKey     = Bool   -- ^ True: stop
type instance ValueType FinalKey      = (Int, Int)

instance ValueSerializable BlockKey
instance ValueSerializable PriorBlockKey
instance ValueSerializable DecideKey
instance ValueSerializable FinalKey

instance ToFileStatKey BlockKey where toFileStatKey = mkFileStatKeyViaJSON
instance ToFileStatKey PriorBlockKey where toFileStatKey = mkFileStatKeyViaJSON
instance ToFileStatKey DecideKey where toFileStatKey = mkFileStatKeyViaJSON
instance ToFileStatKey FinalKey where toFileStatKey = mkFileStatKeyViaJSON

type instance DepKeys BlockKey      = '[PriorBlockKey]
type instance DepKeys PriorBlockKey = '[]
type instance DepKeys DecideKey     = '[BlockKey]
type instance DepKeys FinalKey      = '[BlockKey]

blockKeys :: Search -> Int -> [BlockKey]
blockKeys search r = [ MkBlockKey { round = r, index = i } | i <- [1 .. search.numBlocks] ]

instance ComputeValue BlockKey where
  computeValue _ _ key
    | key.round == 0 = pure (initialValue key.index)
    | otherwise      = (`div` 2) <$> fetch (MkPriorBlockKey MkBlockKey { round = key.round - 1, index = key.index })

-- | A block is read by the next round's block, which is added later.
instance TaskKey BlockKey where
  type TaskConfig BlockKey = Search
  memoryEstimate _ _ = 10 * 1024 * 1024
  keepOutputs _ = True
  tag _ = Just "Block"

-- | Only a dependency, never a task of its own.
instance TaskKey PriorBlockKey where
  type TaskConfig PriorBlockKey = Search
  taskKind = PlaceholderTask
  tag _ = Just "PriorBlock"

-- | The decision reads every block of its round, decides, and adds the next
-- round's decision (whose map reaches the next round's blocks) or the final
-- task. The only task with follow-ups.
decideTask
  :: (Applicative f, FetchesPath DecideKey f, FetchesPath BlockKey f)
  => NumCPUs -> Search -> DecideKey -> f (TaskHandle DecideKey -> Process ())
decideTask _ search key = do
  path <- getPath key
  getValues <- unWrappedProcess $ traverse fetch (blockKeys search key.round)
  pure $ \handle -> do
    values <- getValues
    let stop = maximum values < search.threshold
    Log.info "Decision (round, values, stop)" (key.round, values, stop)
    if stop
      then addFollowUp handle (VRight (VLeft MkFinalKey { round = key.round }))
      else addFollowUp handle (VLeft MkDecideKey { round = key.round + 1 })
    liftIO $ saveValue key path stop

instance TaskKey DecideKey where
  type TaskConfig DecideKey = Search
  type FollowUps DecideKey = '[DecideKey, FinalKey]
  taskKind = CustomTaskWithHandle decideTask
  memoryEstimate _ _ = 10 * 1024 * 1024
  tag _ = Just "Decide"

instance ComputeValue FinalKey where
  computeValue _ search key = do
    values <- traverse fetch (blockKeys search key.round)
    pure (key.round, sum values)

instance TaskKey FinalKey where
  type TaskConfig FinalKey = Search
  memoryEstimate _ _ = 10 * 1024 * 1024
  tag _ = Just "Final"

instance Static (Binary BlockKey) where closureDict = static Dict
instance Static (Binary PriorBlockKey) where closureDict = static Dict
instance Static (Binary DecideKey) where closureDict = static Dict
instance Static (Binary FinalKey) where closureDict = static Dict
instance Static (TaskKey BlockKey) where closureDict = static Dict
instance Static (TaskKey PriorBlockKey) where closureDict = static Dict
instance Static (TaskKey DecideKey) where closureDict = static Dict
instance Static (TaskKey FinalKey) where closureDict = static Dict

---------- The resolver: chosen where the run is set up, seen by no task ----------

data Dirs = MkDirs
  { dataDir   :: OsPath   -- ^ block values and decisions (shared or node-local)
  , resultDir :: OsPath   -- ^ the final result, always shared
  } deriving (Eq, Ord, Show, Generic, Binary, ToJSON)

instance PathResolver Dirs BlockKey where
  resolvePath dirs key = dirs.dataDir </> "block_" <> showOs key.round <> "_" <> showOs key.index <.> "bin"

instance PathResolver Dirs PriorBlockKey where
  resolvePath dirs (MkPriorBlockKey key) = resolvePath dirs key

instance PathResolver Dirs DecideKey where
  resolvePath dirs key = dirs.dataDir </> "decision_" <> showOs key.round <.> "bin"

instance PathResolver Dirs FinalKey where
  resolvePath dirs _ = dirs.resultDir </> "final" <.> "bin"

instance Static (Binary Dirs) where closureDict = static Dict
instance Static (PathResolver Dirs BlockKey) where closureDict = static Dict
instance Static (PathResolver Dirs PriorBlockKey) where closureDict = static Dict
instance Static (PathResolver Dirs DecideKey) where closureDict = static Dict
instance Static (PathResolver Dirs FinalKey) where closureDict = static Dict

---------- The test ----------

data FollowUpKeysProblem = MkFollowUpKeysProblem
  { numBlocks :: Int
  , threshold :: Int
  , nodeCpus  :: Int
  , nodeLocal :: Bool   -- ^ block values and decisions under the node-local storage path
  } deriving (Eq, Ord, Show, Generic, Binary)

instance Static (Binary FollowUpKeysProblem) where closureDict = static Dict

-- | One scenario, run inside a job (top-level so that it can be referenced
-- with @static@).
followUpKeysJob :: Job Scheduler.Config -> OsPath -> FollowUpKeysProblem -> Job ()
followUpKeysJob getSchedulerConfig baseDir problem = do
  schedulerConfig <- getSchedulerConfig
  let
    name = "followupkeys_" <> showOs problem.numBlocks <> "_blocks" <> (if problem.nodeLocal then "_local" else "_shared")
    resultDir = baseDir </> name
    dirs = MkDirs
      { dataDir   = if problem.nodeLocal then schedulerConfig.localStoragePath </> name else resultDir </> "rounds"
      , resultDir = resultDir
      }
    search = MkSearch { numBlocks = problem.numBlocks, threshold = problem.threshold }
    (expectedRound, expectedSum) = expectedResult search
  Log.info "Cleaning/creating" resultDir
  liftIO $ do
    removePathForcibly resultDir
    createDirectoryIfMissing True resultDir
  Log.info "Run follow-up keys test for (problem, expected round, expected sum)" (problem, expectedRound, expectedSum)
  taskMap <- mkTaskMap dirs (CCons search CNil) MkDecideKey { round = 0 }
  records <- runTasks schedulerConfig taskMap
  writeTaskStats (resultDir </> "task_stats.json") (foldMap recordToTaskStats records)
  result <- liftIO $ readValue (MkFinalKey expectedRound) (resolvePath dirs (MkFinalKey expectedRound))
  Log.info "Final result (round, sum)" result
  unless (result == (expectedRound, expectedSum)) $
    Log.throw $ AssertionFailed $
      "Wrong result: expected " <> show (expectedRound, expectedSum) <> ", got " <> show result
  let expectedCount = expectedTaskCount search expectedRound
  Log.info "Task records (got, expected)" (length records, expectedCount)
  unless (length records == expectedCount) $
    Log.throw $ AssertionFailed $
      "Wrong number of task records: expected " <> show expectedCount <> ", got " <> show (length records)
  if problem.nodeLocal
    then do
      leftovers <- liftIO $ filterM doesFileExist $
        [ resolvePath dirs key | r <- [0 .. expectedRound], key <- blockKeys search r ]
        ++ [ resolvePath dirs (MkDecideKey r) | r <- [0 .. expectedRound] ]
      Log.info "Node-local files left after the run" (length leftovers)
      unless (null leftovers) $
        Log.throw $ AssertionFailed $ "Node-local files left after the run: " <> show leftovers
    else pure ()
  Log.info "Follow-up keys test passed" problem

defaultProblems :: [FollowUpKeysProblem]
defaultProblems =
  [ MkFollowUpKeysProblem { numBlocks = 4, threshold = 10, nodeCpus = 8, nodeLocal = False }
  , MkFollowUpKeysProblem { numBlocks = 4, threshold = 10, nodeCpus = 8, nodeLocal = True }
  ]
