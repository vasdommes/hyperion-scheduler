{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StaticPointers        #-}

-- | Test of tasks added during a run ("Hyperion.Scheduler.Dynamic").
--
-- A search in rounds, all inside one 'runTasks'. Round @r@ has one @Block@
-- task per block, which halves the block's value, and one @Decide@ task that
-- depends on all of them, reads the values and either adds round @r+1@ (the
-- next blocks and the next decision) or, once every value is below the
-- threshold, adds a @Final@ task that sums them. The initial task map
-- contains only round 0. The test checks the final round and sum against a
-- direct computation, and the number of task records against the number of
-- tasks the search must have created.
--
-- Each block writes two files: its value, read by the decision task, and a
-- copy of it, read only by the block of the next round. With node-local
-- storage the copy has no known reader when the block finishes, so the block
-- declares 'taskKeepOutputs'; the scheduler would otherwise delete the file
-- and refuse the next round's block. The test runs once with outputs on the
-- shared file system and once with node-local outputs, and checks in the
-- second case that no node-local file survives the run.
module Hyperion.Scheduler.Test.FollowUps where

import Control.Exception              (AssertionFailed (..))
import Control.Monad                  (filterM, forM, unless)
import Control.Monad.IO.Class         (liftIO)
import Data.Aeson                     (ToJSON)
import Data.Binary                    (Binary)
import Data.ByteString.Char8          qualified as BS8
import Data.Map.Strict                qualified as Map
import Data.Set                       qualified as Set
import GHC.Generics                   (Generic)
import Hyperion
import Hyperion.Log                   qualified as Log
import Hyperion.OsPath                (OsPath, (<.>), (</>))
import Hyperion.OsString              (showOs)
import Hyperion.Scheduler             (IsTask (..), Lease (..),
                                       TaskKeyFileInfo (..), ToStatKey (..),
                                       addTasks, mkFileStatKeyViaJSON,
                                       mkStatKeyViaJSON, recordToTaskStats,
                                       runTasks, writeTaskStats)
import Hyperion.Scheduler.Config      qualified as Scheduler
import Hyperion.Scheduler.FilePath    (VirtualFilePath (..))
import System.Directory.OsPath        (createDirectoryIfMissing,
                                       doesFileExist, removePathForcibly)
import System.File.OsPath             qualified as File

---------- The search ----------

-- | Shared by every task of one search.
data Search = MkSearch
  { numBlocks :: Int
  , threshold :: Int
  , dataDir   :: OsPath   -- ^ where the round files go (shared or node-local)
  , resultDir :: OsPath   -- ^ where the final result goes (always shared)
  , keepState :: Bool     -- ^ blocks declare 'taskKeepOutputs' for their state files
  } deriving (Eq, Ord, Show, Generic, Binary, ToJSON)

initialValue :: Int -> Int
initialValue i = 1000 + 37 * i

-- | The value of block @i@ after @r@ halvings.
valueAt :: Int -> Int -> Int
valueAt r i = iterate (`div` 2) (initialValue i) !! r

-- | The round at which the search stops, and the sum it reports.
expectedResult :: Search -> (Int, Int)
expectedResult search = go 0
  where
    values r = map (valueAt r) [1 .. search.numBlocks]
    go r
      | maximum (values r) < search.threshold = (r, sum (values r))
      | otherwise                             = go (r + 1)

-- | Tasks of the search created for the given final round.
expectedTaskCount :: Search -> Int -> Int
expectedTaskCount search finalRound =
  (finalRound + 1) * search.numBlocks   -- blocks
  + (finalRound + 1)                     -- decisions
  + 1                                    -- final

data RoundTask
  = Block  { search :: Search, round :: Int, index :: Int }
  | Decide { search :: Search, round :: Int }
  | Final  { search :: Search, round :: Int }
  deriving (Eq, Ord, Show, Generic, Binary, ToJSON)

valuePath, statePath :: Search -> Int -> Int -> OsPath
valuePath s r i = s.dataDir </> "value_" <> showOs r <> "_" <> showOs i <.> "txt"
statePath s r i = s.dataDir </> "state_" <> showOs r <> "_" <> showOs i <.> "txt"

decisionPath :: Search -> Int -> OsPath
decisionPath s r = s.dataDir </> "decision_" <> showOs r <.> "txt"

finalPath :: Search -> OsPath
finalPath s = s.resultDir </> "final.txt"

fileInfo :: Show a => a -> OsPath -> TaskKeyFileInfo
fileInfo key path = MkTaskKeyFileInfo
  { fileStatKey = mkFileStatKeyViaJSON (show key)
  , path        = VirtualFilePath path
  , fileSize    = 16
  }

writeInt :: OsPath -> Int -> Process ()
writeInt path n = liftIO $ File.writeFile' path (BS8.pack (show n))

readInt :: OsPath -> Process Int
readInt path = liftIO $ read . BS8.unpack <$> File.readFile' path

blocks :: Search -> Int -> [RoundTask]
blocks s r = [Block { search = s, round = r, index = i } | i <- [1 .. s.numBlocks]]

-- | The task map of one round: its blocks and the decision that follows them.
roundTasks :: Search -> Int -> Map.Map RoundTask (Set.Set RoundTask)
roundTasks s r =
  Map.insert Decide { search = s, round = r } (Set.fromList (blocks s r)) $
  Map.fromList
    [ (b, if r == 0 then Set.empty else Set.singleton b { round = r - 1 })
    | b <- blocks s r ]

roundTaskClosure :: Lease -> RoundTask -> Process ()
roundTaskClosure lease t = do
  liftIO $ createDirectoryIfMissing True t.search.dataDir
  case t of
    Block{} -> do
      value <-
        if t.round == 0
          then pure (initialValue t.index)
          else (`div` 2) <$> readInt (statePath t.search (t.round - 1) t.index)
      writeInt (valuePath t.search t.round t.index) value
      writeInt (statePath t.search t.round t.index) value
    Decide{} -> do
      values <- forM (blocks t.search t.round) $ \b -> readInt (valuePath b.search b.round b.index)
      let
        s = t.search
        stop = maximum values < s.threshold
        next
          | stop      = Map.singleton Final { search = s, round = t.round } (Set.fromList (blocks s t.round))
          | otherwise = roundTasks s (t.round + 1)
      Log.info "Decision (round, values, stop)" (t.round, values, stop)
      -- Before this task finishes, so that the run cannot end without them.
      addTasks lease next
      writeInt (decisionPath s t.round) (if stop then 1 else 0)
    Final{} -> do
      values <- forM (blocks t.search t.round) $ \b -> readInt (valuePath b.search b.round b.index)
      liftIO $ createDirectoryIfMissing True t.search.resultDir
      liftIO $ File.writeFile' (finalPath t.search) (BS8.pack (show (t.round, sum values)))
      Log.info "Final (round, sum)" (t.round, sum values)

instance IsTask RoundTask where
  taskMemoryEstimate _ = 10 * 1024 * 1024
  taskInputs t = case t of
    Block{}
      | t.round == 0 -> Set.empty
      | otherwise    -> Set.singleton (fileInfo t (statePath t.search (t.round - 1) t.index))
    Decide{} -> Set.fromList [fileInfo b (valuePath b.search b.round b.index) | b <- blocks t.search t.round]
    Final{}  -> Set.fromList [fileInfo b (valuePath b.search b.round b.index) | b <- blocks t.search t.round]
  taskOutputs t = case t of
    Block{}  -> Set.fromList
      [ fileInfo (t, "value" :: String) (valuePath t.search t.round t.index)
      , fileInfo (t, "state" :: String) (statePath t.search t.round t.index) ]
    Decide{} -> Set.singleton (fileInfo t (decisionPath t.search t.round))
    Final{}  -> Set.singleton (fileInfo t (finalPath t.search))
  -- The state file has no reader yet when the block finishes.
  taskKeepOutputs t = case t of
    Block{} -> t.search.keepState
    _       -> False
  taskTag Block{}  = Just "Block"
  taskTag Decide{} = Just "Decide"
  taskTag Final{}  = Just "Final"
  taskClosure _ _ = Nothing
  taskClosureWithLease lease t = Just $ static roundTaskClosure `cAp` cPure lease `cAp` cPure t

instance ToStatKey RoundTask where
  toStatKey = mkStatKeyViaJSON

instance Static (Binary RoundTask) where closureDict = static Dict
instance Static (IsTask RoundTask) where closureDict = static Dict

---------- The test ----------

data FollowUpsProblem = MkFollowUpsProblem
  { numBlocks :: Int
  , threshold :: Int
  , nodeCpus  :: Int
  , nodeLocal :: Bool   -- ^ round files under the node-local storage path
  } deriving (Eq, Ord, Show, Generic, Binary)

instance Static (Binary FollowUpsProblem) where closureDict = static Dict

-- | One scenario of the test, run inside a job (top-level so that it can be
-- referenced with @static@).
followUpsJob :: Job Scheduler.Config -> OsPath -> FollowUpsProblem -> Job ()
followUpsJob getSchedulerConfig baseDir problem = do
  schedulerConfig <- getSchedulerConfig
  let
    name = "followups_" <> showOs problem.numBlocks <> "_blocks" <> (if problem.nodeLocal then "_local" else "_shared")
    resultDir = baseDir </> name
    dataDir
      | problem.nodeLocal = schedulerConfig.localStoragePath </> name
      | otherwise         = resultDir </> "rounds"
    search = MkSearch
      { numBlocks = problem.numBlocks
      , threshold = problem.threshold
      , dataDir   = dataDir
      , resultDir = resultDir
      , keepState = problem.nodeLocal
      }
    (expectedRound, expectedSum) = expectedResult search
  Log.info "Cleaning/creating" resultDir
  liftIO $ do
    removePathForcibly resultDir
    createDirectoryIfMissing True resultDir
  Log.info "Run follow-ups test for (problem, expected round, expected sum)" (problem, expectedRound, expectedSum)
  records <- runTasks schedulerConfig (roundTasks search 0)
  writeTaskStats (resultDir </> "task_stats.json") (foldMap recordToTaskStats records)
  result <- liftIO $ read . BS8.unpack <$> File.readFile' (finalPath search)
  Log.info "Final result (round, sum)" (result :: (Int, Int))
  unless (result == (expectedRound, expectedSum)) $
    Log.throw $ AssertionFailed $
      "Wrong result: expected " <> show (expectedRound, expectedSum) <> ", got " <> show result
  let expectedCount = expectedTaskCount search expectedRound
  Log.info "Task records (got, expected)" (length records, expectedCount)
  unless (length records == expectedCount) $
    Log.throw $ AssertionFailed $
      "Wrong number of task records: expected " <> show expectedCount <> ", got " <> show (length records)
  -- Node-local files, kept or not, must all be gone at the end of the run.
  if problem.nodeLocal
    then do
      leftovers <- liftIO $ filterM doesFileExist
        [ path
        | r <- [0 .. expectedRound], i <- [1 .. search.numBlocks]
        , path <- [valuePath search r i, statePath search r i] ]
      Log.info "Node-local files left after the run" (length leftovers)
      unless (null leftovers) $
        Log.throw $ AssertionFailed $ "Node-local files left after the run: " <> show leftovers
    else pure ()
  Log.info "Follow-ups test passed" problem

-- | The scenarios the driver runs: the same search with the round files on
-- the shared file system and under the node-local storage path.
defaultProblems :: [FollowUpsProblem]
defaultProblems =
  [ MkFollowUpsProblem { numBlocks = 4, threshold = 10, nodeCpus = 8, nodeLocal = False }
  , MkFollowUpsProblem { numBlocks = 4, threshold = 10, nodeCpus = 8, nodeLocal = True }
  ]
