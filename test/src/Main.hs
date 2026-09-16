{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StaticPointers        #-}

-- | Driver for the LinearTransform scheduler test.
--
-- Three entry points:
--
-- > hyperion-scheduler-test local  [--shift N] [--dim N] [--base-dir DIR] [--cpus N]
-- > hyperion-scheduler-test master [--shift N] [--dim N] [--base-dir DIR] <sbatch options>
-- > hyperion-scheduler-test worker ...   -- launched by hyperion, not by hand
--
-- @local@ runs everything in this process with no SLURM and no cluster
-- database; @master@ and @worker@ are hyperion's usual cluster entry points.
--
-- The HPC site for the cluster entry points comes from the
-- @HYPERION_SCHEDULER_TEST_SITE@ environment variable (see 'siteEnvVar'), not from a
-- command-line flag: workers are launched by hyperion without our options, and
-- master and workers must agree on 'HyperionStaticConfig'. The environment is
-- inherited through @sbatch@, a flag would not be.
module Main where

import Control.Monad                           (forM_)
import Control.Monad.IO.Class                  (liftIO)
import Control.Monad.Reader                    (local)
import Data.Maybe                              (fromMaybe)
import GHC.Conc                                (getNumProcessors)
import Hyperion                                hiding (opts)
import Hyperion.Log                            qualified as Log
import Hyperion.OsPath                         (OsPath, (</>))
import Hyperion.OsString                       (fromString, showOs)
import Hyperion.Util                           (minute)
import Hyperion.Scheduler.Test.Config          (Site)
import Hyperion.Scheduler.Test.Config          qualified as TestConfig
import Hyperion.Scheduler.Test.FollowUpKeys    qualified as FollowUpKeys
import Hyperion.Scheduler.Test.FollowUps       qualified as FollowUps
import Hyperion.Scheduler.Test.LinearTransform (CyclicShiftProblem (..),
                                                linearTransformJob)
import Hyperion.Slurm                          (SbatchOptions (..),
                                                sBatchOptionsParser)
import Options.Applicative                     (Parser, ParserInfo, auto,
                                                execParser, fullDesc, help,
                                                helper, info, long, metavar,
                                                option, optional, progDesc, str,
                                                value, (<**>))
import System.Console.Concurrent               (withConcurrentOutput)
import System.Directory.OsPath                 (createDirectoryIfMissing,
                                                makeAbsolute)
import System.Environment                      (getArgs, lookupEnv, withArgs,
                                                withProgName)
import System.Exit                             (die, exitSuccess)

-- * Options

-- | Options shared by the local and cluster entry points.
data ProblemOptions = ProblemOptions
  { shift   :: Int
  , dim     :: Int
  , baseDir :: Maybe OsPath
  } deriving (Show)

problemOptsParser :: Parser ProblemOptions
problemOptsParser = do
  shift <- option auto $ long "shift" <> metavar "INT" <> value 2
    <> help "Shift the input vector cyclically by this many positions (one layer per position)"
  dim <- option auto $ long "dim" <> metavar "INT" <> value 3
    <> help "Input vector size"
  baseDir <- optional $ option (fromString <$> str) $ long "base-dir" <> metavar "DIR"
    <> help "Base directory for output files"
  pure ProblemOptions{..}

toProblem :: ProblemOptions -> CyclicShiftProblem
toProblem opts = MkCyclicShiftProblem { shift = opts.shift, dim = opts.dim }

-- | @local@ options: 'ProblemOptions' plus the size of the fake node.
data LocalOptions = LocalOptions
  { problem :: ProblemOptions
  , cpus    :: Maybe Int
  } deriving (Show)

localOptsParser :: Parser LocalOptions
localOptsParser = do
  problem <- problemOptsParser
  cpus <- optional $ option auto $ long "cpus" <> metavar "INT"
    <> help "CPUs to give the local node (default: all available)"
  pure LocalOptions{..}

localOptsInfo :: ParserInfo LocalOptions
localOptsInfo = info (localOptsParser <**> helper) $ fullDesc
  <> progDesc "Run the LinearTransform test in this process, without SLURM"

-- | @master@ options: 'ProblemOptions' plus the usual @sbatch@ options.
data MasterOptions = MasterOptions
  { problem       :: ProblemOptions
  , sbatchOptions :: SbatchOptions
  } deriving (Show)

masterOptsParser :: Parser MasterOptions
masterOptsParser = do
  problem <- problemOptsParser
  sbatchOptions <- sBatchOptionsParser
  pure MasterOptions{..}

-- * Local runs

-- | Default base directory for @local@, relative to the current directory.
defaultLocalBaseDir :: OsPath
defaultLocalBaseDir = "tmp/hyperion-scheduler-linear-transform-test"

-- | 'withConcurrentOutput' is needed because 'Log' writes through
-- "System.Console.Concurrent", which otherwise drops buffered output at exit.
-- 'hyperionMain' does the same for the cluster entry points.
runLocal :: LocalOptions -> IO ()
runLocal opts = withConcurrentOutput $ do
  baseDir <- makeAbsolute $ fromMaybe defaultLocalBaseDir opts.problem.baseDir
  cpus <- maybe getNumProcessors pure opts.cpus
  createDirectoryIfMissing True baseDir
  let
    -- 'runJobLocal' needs a 'ProgramInfo'; point it inside baseDir so a local
    -- run leaves nothing behind in the current directory.
    programInfo = ProgramInfo
      { programId       = ProgramId "local"
      , programDatabase = baseDir </> "local.sqlite"
      , programLogDir   = baseDir </> "logs"
      , programDataDir  = baseDir </> "data"
      }
    schedulerConfig = TestConfig.localSchedulerConfig (baseDir </> "node_local_storage")
  Log.info "Running locally" (baseDir, cpus)
  runJobLocal defaultHyperionStaticConfig programInfo $
    -- 'runJobLocal' hardcodes one CPU; 'getJobNodes' reads this to size the node.
    local (\env -> env { jobNodeCpus = NumCPUs cpus }) $
      linearTransformJob (pure schedulerConfig) baseDir (toProblem opts.problem)

-- * Cluster runs

-- | Environment variable naming the HPC site, e.g. @HYPERION_SCHEDULER_TEST_SITE=expanse@.
siteEnvVar :: String
siteEnvVar = "HYPERION_SCHEDULER_TEST_SITE"

getSite :: IO Site
getSite = lookupEnv siteEnvVar >>= \case
  Nothing -> pure TestConfig.defaultSite
  Just s  -> either (die . ((siteEnvVar <> ": ") <>)) pure (TestConfig.parseSite s)

-- | Run a cluster test through 'hyperionMain' with the master options: the
-- given computation gets the options and the absolute base directory.
runClusterTest :: Site -> (MasterOptions -> OsPath -> Cluster ()) -> IO ()
runClusterTest site computation =
  hyperionMain masterOptsParser mkHyperionConfig (TestConfig.hyperionStaticConfig site) clusterComputation
  where
    mkHyperionConfig opts =
      TestConfig.getHyperionConfig site (fromMaybe "." opts.problem.baseDir) opts.sbatchOptions
    clusterComputation opts = do
      baseDir <- liftIO $ maybe (TestConfig.getScratchDir site) makeAbsolute opts.problem.baseDir
      computation opts baseDir

runOnCluster :: Site -> IO ()
runOnCluster site = runClusterTest site $ \opts baseDir ->
  remoteEvalJob $ static linearTransformJob
    `cAp` (static TestConfig.getSchedulerConfig `cAp` cPure site)
    `cAp` cPure (workDir baseDir opts)
    `cAp` cPure (toProblem opts.problem)
  where
    -- Keep runs with different allocations side by side, so their task stats
    -- can be compared.
    workDir baseDir opts = baseDir </>
      "nodes_" <> showOs opts.sbatchOptions.nodes <>
      "_ntasks_" <> showOs opts.sbatchOptions.nTasksPerNode

-- | The follow-ups test (tasks that add tasks) on the cluster: every
-- scenario of 'FollowUps.defaultProblems' in turn, one SLURM job each, all
-- inside one 'hyperionMain'. The same executable serves as the worker, and
-- its 'main' must not reach 'hyperionMain' more than once: the worker branch
-- returns when its job is done, and a second call would start a second
-- worker against a master that has already finished. The problem options of
-- 'masterOptsParser' are accepted and ignored, except @--base-dir@.
runFollowUpsOnCluster :: Site -> IO ()
runFollowUpsOnCluster site = runClusterTest site $ \_ baseDir ->
  forM_ FollowUps.defaultProblems $ \problem ->
    local (setJobTime (20 * minute) . setJobType (MPIJob 1 problem.nodeCpus)) $
      remoteEvalJob $ static FollowUps.followUpsJob
        `cAp` (static TestConfig.getSchedulerConfig `cAp` cPure site)
        `cAp` cPure (baseDir </> "followups" </> "mpi_1_" <> showOs problem.nodeCpus)
        `cAp` cPure problem

-- | The same search with 'TaskKey' keys and follow-ups declared by the
-- decision key ('Hyperion.Scheduler.Test.FollowUpKeys'), one SLURM job per
-- scenario.
runFollowUpKeysOnCluster :: Site -> IO ()
runFollowUpKeysOnCluster site = runClusterTest site $ \_ baseDir ->
  forM_ FollowUpKeys.defaultProblems $ \problem ->
    local (setJobTime (20 * minute) . setJobType (MPIJob 1 problem.nodeCpus)) $
      remoteEvalJob $ static FollowUpKeys.followUpKeysJob
        `cAp` (static TestConfig.getSchedulerConfig `cAp` cPure site)
        `cAp` cPure (baseDir </> "followupkeys" </> "mpi_1_" <> showOs problem.nodeCpus)
        `cAp` cPure problem

-- * Entry point

usage :: String
usage = unlines
  [ "Usage: hyperion-scheduler-test COMMAND [OPTIONS]"
  , ""
  , "Commands:"
  , "  local   Run the LinearTransform test in this process, without SLURM"
  , "  master  Run the LinearTransform test on a SLURM cluster"
  , "  followups  Run the tasks-that-add-tasks test (Test.FollowUps) on a SLURM cluster"
  , "  followupkeys  Run the follow-ups-declared-by-a-key test (Test.FollowUpKeys) on a SLURM cluster"
  , "  worker  Run a worker process (launched automatically by the master)"
  , ""
  , "Pass --help after a command for its options."
  , "Set " <> siteEnvVar <> " to select the HPC site for master/worker."
  ]

-- | Select the test with HYPERION_SCHEDULER_TEST=linear (default) or
-- =followups.
main :: IO ()
main = getArgs >>= \case
  "local" : rest    -> withProgName "hyperion-scheduler-test local" $
                       withArgs rest $ execParser localOptsInfo >>= runLocal
  "followups" : rest -> withArgs rest $ getSite >>= runFollowUpsOnCluster
  "followupkeys" : rest -> withArgs rest $ getSite >>= runFollowUpKeysOnCluster
  args | needsUsage -> putStr usage >> exitSuccess
       | otherwise  -> getSite >>= runOnCluster
    where needsUsage = null args || head args `elem` ["-h", "--help", "help"]
