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
{-# LANGUAGE UndecidableInstances  #-}

-- | Scheduler test computing a chain of linear transformations
-- @x_{i+1} = A_i . x_i@, with one task per product @A_ik x_k@.
-- The point is to generate many small tasks with many dependencies;
-- this is not how you should multiply a matrix in practice.
--
-- This module defines the problem only. The driver ("Main") supplies the
-- 'Scheduler.Config' and the base directory, so nothing here knows about
-- SLURM or the command line.
module Hyperion.Scheduler.Test.LinearTransform where

import Bootstrap.Build           (Fetches (..))
import Control.Exception         (AssertionFailed (..))
import Control.Monad             (unless)
import Control.Monad.IO.Class    (liftIO)
import Data.Aeson                (ToJSON)
import Data.Binary               (Binary)
import Data.Matrix               (Matrix)
import Data.Matrix               qualified as Matrix
import Data.Traversable          (for)
import Data.Typeable             (Typeable)
import Data.Vector               (Vector)
import Data.Vector               qualified as Vector
import GHC.Generics              (Generic)
import Hyperion
import Hyperion.Log              qualified as Log
import Hyperion.OsPath           (OsPath, (<.>), (</>))
import Hyperion.OsString         (showOs)
import Hyperion.Scheduler        (PathResolver (..), ToFileStatKey (..),
                                  ToStatKey (..), encodeJsonFileAtomic,
                                  mkStatKeyViaJSON, recordToTaskStats,
                                  writeTaskStats)
import Hyperion.Scheduler        qualified as Scheduler
import Hyperion.Scheduler.Config qualified as Scheduler
import Hyperion.Scheduler.Task   (ComputeValue (..), DepKeys, FetchesKey,
                                  TaskKey (..), TaskKind (..),
                                  ValueSerializable (..),
                                  ValueSerializableM (..), ValueType, getPath,
                                  mkTaskMap)
import System.Directory.OsPath   (createDirectoryIfMissing, removePathForcibly)

-- * The problem

-- | A chain of 'numLayers' matrices applied to 'inputVector'.
class (ToJSON a, Ord a, Show a, Binary a, Typeable a) => LinearTransformContext a where
  numLayers :: a -> Int
  layerMatrix :: a -> Closure (Int -> Matrix Int)
  inputVector :: a -> Closure (Vector Int)
  layerMatrixDims :: Int -> a -> (Int, Int)

layerInputVectorLength :: LinearTransformContext a => Int -> a -> Int
layerInputVectorLength layerIndex ctx = ncols
  where (_nrows, ncols) = layerMatrixDims layerIndex ctx

layerOutputVectorLength :: LinearTransformContext a => Int -> a -> Int
layerOutputVectorLength layerIndex ctx = nrows
  where (nrows, _ncols) = layerMatrixDims layerIndex ctx

-- * Key types

-- | A single product @A_ik x_k@.
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

-- | One element of an output vector, @sum_k A_ik x_k@.
data VectorElementKey a = MkVectorElementKey
  { layerIndex :: Int
  , index      :: Int
  , ctx        :: a
  }
  deriving (Eq, Ord, Show, Generic, Binary, ToJSON, ValueSerializable)

instance (Typeable a, Static (Binary a)) => Static (Binary (VectorElementKey a)) where
  closureDict = static (\Dict -> Dict) `cAp` closureDict @(Binary a)

type instance ValueType (VectorElementKey a) = Int

type instance DepKeys (VectorElementKey a) = '[MultiplyKey a]

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

-- | Pretend this shells out to @vector_element.sh in_1.bin .. in_n.bin out.bin@.
-- It exists to exercise 'CustomTask', i.e. the case where we cannot define
-- @instance ComputeValue (VectorElementKey a)@.
runVectorElementScript :: [(MultiplyKey a, OsPath)] -> (VectorElementKey a, OsPath) -> Process ()
runVectorElementScript inputs (outputKey, outputPath) = do
  Log.info "runVectorElementScript" (map snd inputs, outputPath)
  values <- mapM (uncurry readValueM) inputs
  saveValueM outputKey outputPath (sum values)

instance
  ( LinearTransformContext a
  , ToFileStatKey (MultiplyKey a)
  , ToFileStatKey (VectorElementKey a)
  ) => TaskKey (VectorElementKey a) where
  memoryEstimate _ = 1024 * 1024 * 10 -- TODO: memory estimate
  tag _ = Just "VectorElement"
  taskKind = CustomTask $ \_numCpus _config key -> do
    let keys = vectorElementInputKeys key
    inputs <- zip keys <$> for keys getPath
    outputPath <- getPath key
    pure $ do
      liftIO $ Log.info "Computing vector element" (key, inputs, outputPath)
      runVectorElementScript inputs (key, outputPath)

instance LinearTransformContext a => ToFileStatKey (VectorElementKey a) where
  toFileSize = const 1
  -- TODO toFileStatKey

-- | The vector @x_i@ entering layer @i@.
data VectorKey a = MkVectorKey
  { layerIndex :: Int
  , length     :: Int
  , ctx        :: a
  }
  deriving (Eq, Ord, Show, Generic, Binary, ToJSON, ValueSerializable)

instance (Typeable a, Static (Binary a)) => Static (Binary (VectorKey a)) where
  closureDict = static (\Dict -> Dict) `cAp` closureDict @(Binary a)

type instance ValueType (VectorKey a) = Vector Int

type instance DepKeys (VectorKey a) = '[VectorElementKey a]

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

instance
 ( LinearTransformContext a
 , ToFileStatKey (VectorElementKey a)
 , ToFileStatKey (VectorKey a)
 ) => TaskKey (VectorKey a) where
  memoryEstimate _ = 1024 * 1024
  tag _ = Just "Vector"

instance LinearTransformContext a => ToFileStatKey (VectorKey a) where
  toFileSize key = fromIntegral key.length

newtype VectorStatKey = MkVectorStatKey { size :: Int }
  deriving newtype (ToJSON)

instance LinearTransformContext a => ToStatKey (VectorKey a) where
  toStatKey key = mkStatKeyViaJSON $ MkVectorStatKey { size = layerInputVectorLength key.layerIndex key.ctx }

-- * A concrete problem: cyclic shift

-- | Take the vector @[1..dim]@ and shift it cyclically by @shift@, one
-- position per layer. The output is @[shift+1, .., dim, 1, .., shift]@.
-- @shift@ therefore controls the depth of the task graph and @dim@ its width.
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
  inputVector x = static getInputVector `cAp` cPure x
  layerMatrixDims _layer x = (x.dim, x.dim)

instance Static (LinearTransformContext CyclicShiftProblem) where
  closureDict = static Dict

getInputVector :: CyclicShiftProblem -> Vector Int
getInputVector x = Vector.enumFromN 1 x.dim

getOutputVector :: CyclicShiftProblem -> Vector Int
getOutputVector x = end Vector.++ beg where
  (beg, end) = Vector.splitAt ((-x.shift) `mod` x.dim) $ getInputVector x

-- | Each layer performs a cyclic shift by 1, i.e. 12345 -> 51234
getLayerMatrix :: CyclicShiftProblem -> Int -> Matrix Int
getLayerMatrix x _layer = Matrix.joinBlocks (tl,tr,bl,br) where
  tl = Matrix.zero 1 (x.dim - 1)
  tr = Matrix.identity 1
  bl = Matrix.identity (x.dim - 1)
  br = Matrix.zero (x.dim - 1) 1

-- * Where the task files go

-- | Intermediate values go to node-local storage, the final vector to 'outDir'.
data LinearPathResolver = MkLinearPathResolver
  { outDir  :: OsPath
  , tempDir :: OsPath
  }
  deriving (Generic, Binary, ToJSON, Show, Eq, Ord)

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

-- * Static dictionaries
--
-- Needed because tasks are shipped to workers as closures.

instance Static (Binary LinearPathResolver) where
  closureDict = static Dict

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

-- * The job

-- | Build the task map for @problem@, run it, write task stats under
-- @baseDir@ and check the result against 'getOutputVector'.
--
-- The scheduler config is passed as a 'Job' action (rather than a value) so
-- that it can be evaluated on the node that actually runs the job: on a
-- cluster it reads @SLURM_JOB_ID@ to find node-local storage.
linearTransformJob :: Job Scheduler.Config -> OsPath -> CyclicShiftProblem -> Job ()
linearTransformJob getSchedulerConfig baseDir problem = do
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

  Log.info "Running linear transform test" (problem, resolver)
  liftIO $ do
    removePathForcibly resolver.outDir
    createDirectoryIfMissing True resolver.outDir
  taskMap <- mkTaskMap resolver () outputVectorKey
  taskRecords <- Scheduler.runTasks schedulerConfig taskMap

  let
    taskRecordsFile = resolver.outDir </> "task_records.json"
    taskStatsFile   = resolver.outDir </> "task_stats.json"
  Log.info "Writing task records to file" taskRecordsFile
  encodeJsonFileAtomic taskRecordsFile taskRecords
  writeTaskStats taskStatsFile (foldMap recordToTaskStats taskRecords)

  outputValue <- readValueM outputVectorKey (resolvePath resolver outputVectorKey)
  Log.info "Computed output vector" outputValue
  let expectedValue = getOutputVector problem
  unless (expectedValue == outputValue) $
    Log.throw $ AssertionFailed $
      "Wrong output vector for problem: " <> show problem <>
      ": expected: " <> show expectedValue <>
      ": got: " <> show outputValue
