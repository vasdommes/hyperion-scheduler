{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StaticPointers        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}


-- Scheduler test for computing several consecutive linear transformations:
-- x_{i+1} = A_i . x_i
-- The purpose of this test is to generate many small tasks with multiple dependencies.
-- This not how you should multiuply matrix in practice.
module Hyperion.Scheduler.Test.LinearTransform where

import Bootstrap.Build                (Fetches (..))
import Control.Exception              (AssertionFailed (..))
import Control.Monad                  (unless)
import Control.Monad.IO.Class         (liftIO)
import Control.Monad.Reader           (lift, local)
import Data.Aeson                     (ToJSON, (.=))
import Data.Aeson                     qualified as Aeson
import Data.Binary                    (Binary)
import Data.Kind                      (Type)
import Data.Map.Strict                qualified as Map
import Data.Matrix                    (Matrix)
import Data.Matrix                    qualified as Matrix
import Data.Maybe                     (fromMaybe)
import Data.Proxy                     (Proxy (..))
import Data.Time.Clock                (NominalDiffTime)
import Data.Traversable               (for)
import Data.Typeable                  (Typeable)
import Data.Vector                    (Vector)
import Data.Vector                    qualified as Vector
import Debug.Trace
import GHC.Generics                   (Generic)
import Hyperion
import Hyperion.Log                   qualified as Log
import Hyperion.OsPath                (OsPath, (<.>), (</>))
import Hyperion.OsString              (OsString, fromString, showOs)
import Hyperion.Scheduler             (PathResolver (..), StatKey (..),
                                       TaskChain (..), ToFileStatKey (..),
                                       ToStatKey (..), WrappedTask,
                                       encodeJsonFileAtomic,
                                       memoryToCpuTimeApprox, mkStatKeyViaJSON,
                                       recordToTaskStats, wrapTask,
                                       writeTaskStats)
import Hyperion.Scheduler             qualified as Scheduler
import Hyperion.Scheduler.Config      qualified as Scheduler
import Hyperion.Scheduler.Task        (ComputeValue (..), DepKeys, FetchesKey,
                                       TaskKey (..), ValueSerializable (..),
                                       ValueSerializableM (..), ValueType,
                                       getPath, mkTaskMap)
import Hyperion.Scheduler.Test.Config qualified as TestConfig
import Hyperion.Slurm                 (SbatchOptions, sBatchOptionsParser)
import Hyperion.Util                  (minute)
import Hyperion.Util.MonadPathExists  (MonadPathExists)
import Options.Applicative            (Parser, ReadM, auto, help, long,
                                       maybeReader, metavar, option, optional,
                                       str, value)
import Options.Applicative.Types      (readerAsk)
import System.Directory.OsPath        (createDirectoryIfMissing, makeAbsolute,
                                       removePathForcibly)
import Text.Read                      (Read (..))

class (ToJSON a, Ord a, Show a, Binary a, Typeable a) => LinearTransformContext a where
  numLayers :: a -> Int
  layerMatrix :: a -> Closure (Int -> Matrix Int)
  inputVector :: a -> Closure (Vector Int)
  layerMatrixDims :: Int -> a -> (Int, Int)
  -- layerInputVectorLength :: Int -> a -> Int
  -- layerOutputVectorLength :: Int -> a -> Int
  -- modulus :: a -> Int

layerInputVectorLength :: LinearTransformContext a => Int -> a -> Int
layerInputVectorLength layerIndex ctx = ncols
  where (_nrows, ncols) = layerMatrixDims layerIndex ctx

layerOutputVectorLength :: LinearTransformContext a => Int -> a -> Int
layerOutputVectorLength layerIndex ctx = nrows
  where (nrows, _ncols) = layerMatrixDims layerIndex ctx

-- Key types

-- TODO rename?
-- single element multiplication A_ik x_k
data MultiplyKey a = MkMultiplyKey
  { layerIndex :: Int
  , row        :: Int
  , col        :: Int
  , ctx        :: a
  }
  deriving (Eq, Ord, Show, Generic, Binary, ToJSON, ValueSerializable)

instance (Typeable a, Static (Binary a)) => Static (Binary (MultiplyKey a)) where
  closureDict = static (\Dict -> Dict) `cAp` closureDict @(Binary a)

type instance ValueType (MultiplyKey a) = Int

type instance DepKeys (MultiplyKey a) = '[VectorKey a]


computeMultiplyM :: (Applicative f, FetchesKey (VectorKey a) f, LinearTransformContext a) => MultiplyKey a -> f (Process Int)
computeMultiplyM key = do
  v <- fVector
  pure $ do
    getLayerMatrix' <- unClosure (layerMatrix key.ctx)
    let
      matrix = getLayerMatrix' key.layerIndex
    let
      -- NB: Data.Matrix elements are 1-indexed, but Data.Vector are 0-indexed
      m_ik = Matrix.getElem (key.row + 1) (key.col + 1) matrix
      v_k = v Vector.! key.col
    pure $ m_ik * v_k
  where
    fVector = fetch MkVectorKey
      { layerIndex = key.layerIndex - 1
      , length = layerInputVectorLength key.layerIndex key.ctx
      , ctx = key.ctx
      }

instance LinearTransformContext a => ComputeValue (MultiplyKey a) where
  computeValueM _ _ = computeMultiplyM

type instance DepKeys (MultiplyKey a) = '[VectorKey a]

instance
  ( LinearTransformContext a
  , ToFileStatKey (MultiplyKey a)
  , ToFileStatKey (VectorKey a)
  ) => TaskKey (MultiplyKey a) where
  memoryEstimate _ = 1024 * 1024 * 10 -- TODO: memory estimate
  tag _ = Just "Multiply"

instance LinearTransformContext a => ToFileStatKey (MultiplyKey a) where
  toFileSize = const 1
  -- TODO toFileStatKey

data VectorElementKey a = MkVectorElementKey
  { layerIndex :: Int
  , index      :: Int
  , ctx        :: a
  }
  deriving (Eq, Ord, Show, Generic, Binary, ToJSON, ValueSerializable)

instance (Typeable a, Static (Binary a)) => Static (Binary (VectorElementKey a)) where
  closureDict = static (\Dict -> Dict) `cAp` closureDict @(Binary a)

type instance ValueType (VectorElementKey a) = Int

vectorElementInputKeys :: LinearTransformContext a => VectorElementKey a -> [MultiplyKey a]
vectorElementInputKeys key = map mkKey ks where
  ncols = layerInputVectorLength key.layerIndex key.ctx
  ks = [0..ncols-1]
  mkKey k = MkMultiplyKey
    { layerIndex = key.layerIndex
    , row = key.index
    , col = k
    , ctx = key.ctx
    }


 -- sum_k A_ik x_k
--computeVectorElement :: (Applicative f, FetchesKey (MultiplyKey a) f, LinearTransformContext a) => VectorElementKey a -> f Int
--computeVectorElement key = sum <$> for (vectorElementInputKeys key) fetch

-- instance LinearTransformContext a => ComputeValue (VectorElementKey a) where
--   computeValue _ _ = computeVectorElement

-- Let's pretend that this calls some external script
--   that reads A_ik x_k, computes their sum and write it to the output file:
-- $ vector_element.sh input_1.bin input_2.bin input_3.bin output.bin
-- This is needed to test KeyType for the case
-- when we cannot define instance ComputeValue (VectorElementKey a).
runVectorElementScript :: [(MultiplyKey a, OsPath)] -> (VectorElementKey a, OsPath) -> Process ()
runVectorElementScript inputs (outputKey, outputPath) = do
  Log.info "runVectorElementScript" (map snd inputs, outputPath)
  let readValue' (key, path) = readValueM key path
  values <- mapM readValue' inputs
  let result = sum values
  saveValueM outputKey outputPath result


type instance DepKeys (VectorElementKey a) = '[MultiplyKey a]

instance
  ( LinearTransformContext a
  , ToFileStatKey (MultiplyKey a)
  , ToFileStatKey (VectorElementKey a)
  ) => TaskKey (VectorElementKey a) where
  -- type DepKeys (VectorElementKey a) = '[MultiplyKey a]
  memoryEstimate _ = 1024 * 1024 * 10 -- TODO: memory estimate
  tag _ = Just "VectorElement"
  computeAndSaveValue numCpus config key = do
    let keys = vectorElementInputKeys key
    inputs <- zip keys <$> for keys getPath
    outputPath <- getPath key
    pure $ do
      liftIO $ Log.info "Computing vector element" (key, inputs, outputPath)
      runVectorElementScript inputs (key, outputPath)

newtype VectorStatKey = MkVectorStatKey { size :: Int }
  deriving newtype(ToJSON)

instance LinearTransformContext a => ToStatKey (VectorKey a) where
  toStatKey key = mkStatKeyViaJSON $ MkVectorStatKey { size = layerInputVectorLength key.layerIndex key.ctx }


instance LinearTransformContext a => ToFileStatKey (VectorElementKey a) where
  toFileSize = const 1
  -- TODO toFileStatKey

data VectorKey a = MkVectorKey
  { layerIndex :: Int
  , length     :: Int
  , ctx        :: a
  }
  deriving (Eq, Ord, Show, Generic, Binary, ToJSON, ValueSerializable)

instance (Typeable a, Static (Binary a)) => Static (Binary (VectorKey a)) where
  closureDict = static (\Dict -> Dict) `cAp` closureDict @(Binary a)

type instance ValueType (VectorKey a) = Vector Int

computeVectorM :: (Applicative f, FetchesKey (VectorElementKey a) f, LinearTransformContext a) => VectorKey a -> f (Process (Vector Int))
computeVectorM key
  | key.layerIndex == 0 = pure $ unClosure $ inputVector key.ctx
  | otherwise = fmap pure $ sequenceA $
    Vector.generate key.length $ \i ->
      fetch MkVectorElementKey
        { layerIndex = key.layerIndex
        , index = i
        , ctx = key.ctx
        }

instance LinearTransformContext a => ComputeValue (VectorKey a) where
  computeValueM _ _ = computeVectorM

type instance DepKeys (VectorKey a) = '[VectorElementKey a]

instance
 ( LinearTransformContext a
 , ToFileStatKey (VectorElementKey a)
 , ToFileStatKey (VectorKey a)
 ) => TaskKey (VectorKey a) where
  -- type DepKeys (VectorKey a) = '[VectorElementKey a]
  memoryEstimate _ = 1024 * 1024
  tag _ = Just "Vector"

instance LinearTransformContext a => ToFileStatKey (VectorKey a) where
  toFileSize key = fromIntegral key.length



-- Take vector [1,2,..dim] and make a cyclic shift.
-- The output vector is [shift+1, shift+2,...,dim, 1, 2,...,shift]
-- For stress-testing scheduler,
data CyclicShiftProblem = MkCyclicShiftProblem
  { shift :: Int
  , dim   :: Int
  }
  deriving (Eq, Ord, Generic, Binary, ToJSON, Show)

instance Static (Binary CyclicShiftProblem) where
  closureDict = static Dict
instance Static (ToJSON CyclicShiftProblem) where
  closureDict = static Dict

instance LinearTransformContext CyclicShiftProblem where
  numLayers x = x.shift
  layerMatrix x = static getLayerMatrix `cAp` cPure x
  -- Vector 1..N
  inputVector x = static getInputVector `cAp` cPure x
  layerMatrixDims _layer x = (x.dim, x.dim)

instance Static (LinearTransformContext CyclicShiftProblem) where
  closureDict = static Dict

getInputVector :: CyclicShiftProblem -> Vector Int
getInputVector x = Vector.enumFromN 1 x.dim

getOutputVector :: CyclicShiftProblem -> Vector Int
getOutputVector x = end Vector.++ beg where
  (beg, end) = Vector.splitAt ((-x.shift) `mod` x.dim) $ getInputVector x

-- Each layer performs cyclic shift by 1, permutation 12345 -> 51234
getLayerMatrix :: CyclicShiftProblem -> Int -> Matrix Int
getLayerMatrix x _layer = Matrix.joinBlocks (tl,tr,bl,br) where
  tl = Matrix.zero 1 (x.dim - 1)
  tr = Matrix.identity 1
  bl = Matrix.identity (x.dim - 1)
  br = Matrix.zero (x.dim - 1) 1

data LinearPathResolver = MkLinearPathResolver
  { outDir  :: OsPath
  , tempDir :: OsPath
  }
  deriving (Generic, Binary, ToJSON, Show, Eq, Ord)

instance Typeable a => Static (PathResolver LinearPathResolver (MultiplyKey a)) where
  closureDict = static Dict
instance Typeable a => Static (PathResolver LinearPathResolver (VectorElementKey a)) where
  closureDict = static Dict
instance (Typeable a, Static(LinearTransformContext a)) => Static (PathResolver LinearPathResolver (VectorKey a)) where
  closureDict = static (\Dict -> Dict) `cAp` closureDict @(LinearTransformContext a)

instance (Typeable a, Static(LinearTransformContext a)) => Static (ToFileStatKey (MultiplyKey a)) where
  closureDict = static (\Dict -> Dict) `cAp` closureDict @(LinearTransformContext a)
instance (Typeable a, Static(LinearTransformContext a)) => Static (ToFileStatKey (VectorElementKey a)) where
  closureDict = static (\Dict -> Dict) `cAp` closureDict @(LinearTransformContext a)
instance (Typeable a, Static(LinearTransformContext a)) => Static (ToFileStatKey (VectorKey a)) where
  closureDict = static (\Dict -> Dict) `cAp` closureDict @(LinearTransformContext a)

instance
  ( Typeable a
  , Static(LinearTransformContext a)
  , Static(ToJSON a)
  ) => Static (TaskKey (MultiplyKey a)) where
  closureDict = static (\Dict -> Dict) `cAp` closureDict @(LinearTransformContext a, ToJSON a)

instance
  ( Typeable a
  , Static(LinearTransformContext a)
  , Static(ToJSON a)
  ) => Static (TaskKey (VectorElementKey a)) where
  closureDict = static (\Dict -> Dict) `cAp` closureDict @(LinearTransformContext a, ToJSON a)

instance
  ( Typeable a
  , Static(LinearTransformContext a)
  , Static(ToJSON a)
  ) => Static (TaskKey (VectorKey a)) where
  closureDict = static (\Dict -> Dict) `cAp` closureDict @(LinearTransformContext a, ToJSON a)

instance Static (Binary LinearPathResolver) where
  closureDict = static Dict

instance PathResolver LinearPathResolver (MultiplyKey a) where
  resolvePath r key =
    r.tempDir </>
    "layer_" <> showOs key.layerIndex </>
    "mul_" <> showOs key.row <> "_" <> showOs key.col <.> "bin"

instance PathResolver LinearPathResolver (VectorElementKey a) where
  resolvePath r key =
    r.tempDir </>
    "layer_" <> showOs key.layerIndex </>
    "vec_" <> showOs key.index <.> "bin"

instance (LinearTransformContext a) => PathResolver LinearPathResolver (VectorKey a) where
  resolvePath r key = baseDir </>
    "layer_" <> showOs key.layerIndex </>
    "vec.bin"
    where
      baseDir = if numLayers key.ctx == key.layerIndex
        then r.outDir
        else r.tempDir


localSchedulerConfig :: Scheduler.Config
localSchedulerConfig = Scheduler.MkConfig
  { nodeMemory = 16 * 1024 * 1024 * 1024
  , nodeLocalStorageSize = 2 * 1000 * 1000 * 1000
  , localStoragePath = "/"
  , isLocalPath = const False
  , reportInterval = 5
  }

---- TODO move to Hyperion.OsString
--instance Read OsString where
--  readPrec = fromString <$> readPrec

-- | A datatype for command line arguments
data ProgramOptions = ProgramOptions
  { shift         :: Int
  , dim           :: Int
  , baseDirectory :: Maybe OsPath
  , sbatchOptions :: SbatchOptions
--  -- SLURM options
--  , nodes         :: Int
--  , cpusPerNode   :: Int
--  , memPerNode    :: Maybe OsString
--  , timeLimit     :: NominalDiffTime
--  , partition     :: Maybe OsString
  } deriving (Show)

-- | Parser for command line arguments
programOpts :: Parser ProgramOptions
programOpts = do
  shift <- option auto $ long "shift" <> metavar "INT"
                          <> value 2
                          <> help "Cyclic elements of the input vector by the given number"
  dim <- option auto $ long "dim" <> metavar "INT"
                          <> value 3
                          <> help "Input vector size"
  baseDirectory <- optional $ option auto $ long "base-dir" <> metavar "PATH"
                               <> help "The base directory for writing files"
  sbatchOptions <- sBatchOptionsParser
--  partition <- optional $ option auto (long "partition" <> metavar "STRING"
--                          <> help "SLURM partition")
--  nodes <- option auto (long "nodes" <> metavar "INT"
--                          <> value 1
--                          <> help "SLURM: Number of nodes")
--  cpusPerNode <- option auto (long "ntasks-per-node" <> metavar "INT"
--                          <> value 4
--                          <> help "SLURM: Number of CPUs per node")
--  memPerNode <- optional $ option auto (long "mem" <> metavar "STRING"
--                          <> help "SLURM: memory per node")
--  timeLimit <- option parseTime (long "time" <> metavar "INT"
--                          <> value (5 * minute)
--                          <> help "SLURM: time limit, min")
  pure ProgramOptions{..}
--  where
--    -- convert minutes to seconds
--    parseTime :: ReadM NominalDiffTime
--    parseTime = (minute * ) <$> auto

testJob :: Job Scheduler.Config -> OsPath -> CyclicShiftProblem -> Job ()
testJob getSchedulerConfig baseDir problem = do
  schedulerConfig <- getSchedulerConfig
  let
    relDir = "shift_" <> showOs problem.shift <> "_dim_" <> showOs problem.dim
    resolver = MkLinearPathResolver
      { outDir = baseDir </> relDir
      , tempDir = schedulerConfig.localStoragePath </> relDir
      }
    outputVectorKey = MkVectorKey
      { layerIndex = problem.shift
      , length = problem.dim
      , ctx = problem
      }

  Log.info "Cleaning/creating" resolver.outDir
  Log.info "Run test for " problem
  liftIO $ do
    removePathForcibly resolver.outDir
    createDirectoryIfMissing True resolver.outDir
  taskMap <- mkTaskMap resolver () outputVectorKey
  newTaskRecords <- Scheduler.runTasks schedulerConfig taskMap

  -- Write task stats
  let
    statsDir = resolver.outDir
    taskRecordsFile  = statsDir </> "task_records.json"
    newTaskStatsFile = statsDir </> "task_stats_new.json"
    newTaskStats = foldMap recordToTaskStats newTaskRecords
  Log.info "Writing task records to file" taskRecordsFile
  encodeJsonFileAtomic taskRecordsFile newTaskRecords
  writeTaskStats newTaskStatsFile newTaskStats

  -- Check result
  outputValue <- readValueM outputVectorKey (resolvePath resolver outputVectorKey)
  Log.info "Computed output vector: " outputValue
  let expectedValue = getOutputVector problem
  unless (expectedValue == outputValue) $
    Log.throw $ AssertionFailed $
      "Wrong output vector for problem: " <> show problem <>
      ": expected: " <> show expectedValue <>
      ": got: " <> show outputValue

runTestLocal :: OsPath -> CyclicShiftProblem -> IO ()
runTestLocal baseDir problem = runJobLocal' $ testJob (pure localSchedulerConfig) baseDir problem

runTestSlurm :: TestConfig.HPCName -> OsPath -> CyclicShiftProblem -> IO ()
runTestSlurm hpcName baseDir problem = do
  let
    problemSize = problem.shift * problem.dim * problem.dim
    (partition, jobType, mem, time)
      | problemSize < 1000 = ("debug", MPIJob 2 8, "16G", 5*minute)
--      | otherwise          = ("debug", MPIJob 2 128, "128G", 20*minute)
      | otherwise          = ("debug", MPIJob 2 32, "64G", 30*minute)
--      | otherwise          = ("compute", MPIJob 4 128, "0G", 60*minute)
    workDir = baseDir </> "mpi_" <> showOs jobType.mpiNodes <> "_" <> showOs jobType.mpiNTasksPerNode
  hyperionConfig <- TestConfig.getHyperionConfig hpcName $ Just workDir
  scratchDir <- TestConfig.getScratchDir hpcName
  let
    hyperionStaticConfig = TestConfig.hyperionStaticConfig hpcName
    mkHyperionConfig options = (defaultHyperionConfig $ fromMaybe scratchDir options.baseDirectory)
      { defaultSbatchOptions = options.sbatchOptions }
    clusterComputation options = do
      _ <- local (setJobOptions options.sbatchOptions) $
        remoteEvalJob $ static testJob
          `cAp` (static TestConfig.getSchedulerConfig `cAp` cPure hpcName)
          `cAp` cPure workDir
          `cAp` cPure problem
      pure ()
  hyperionMain programOpts mkHyperionConfig hyperionStaticConfig clusterComputation

runTest :: IO ()
runTest = do
  baseDir <- makeAbsolute "tmp/hyperion-scheduler-linear-transform-test"
  let
    shiftDims = [(10,100)]
--      [ (0,1)
--      , (2,3)
--      , (10,100)
--      ]
    toProblem (shift, dim) = MkCyclicShiftProblem { shift = shift, dim = dim }
  -- mapM_ (runTestLocal baseDir . toProblem) shiftDims
  mapM_ (runTestSlurm "expanse" baseDir . toProblem) shiftDims
