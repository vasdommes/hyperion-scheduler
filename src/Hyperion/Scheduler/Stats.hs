{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StaticPointers        #-}
{-# LANGUAGE TypeFamilies          #-}

module Hyperion.Scheduler.Stats
  ( TaskAndFileStats (..)
  , TaskRecord (..)
  , TaskEstimates (..)
  , TaskStats (..)
  , FileStats (..)
  , Trials (..)
  , toTrials
  , Accuracy (..)
  , taskEstimatesAt
  , estimateAccuracy
  , modelAccuracy
  , accuracyBy
  , fileSizeAccuracy
  , recordToTaskStats
  , approxRuntime
  , maxMemory
  , lookupTaskStats
  , lookupMaxFileSize
  , readTaskStats
  , readTaskRecords
  , writeTaskStats
  , encodeJsonFileAtomic
  ) where

import Control.DeepSeq                (NFData, deepseq)
import Control.Monad.Catch            (Handler (..), catches)
import Control.Monad.IO.Class         (MonadIO, liftIO)
import Data.Aeson                     (AesonException, FromJSON, ToJSON)
import Data.Aeson                     qualified as Aeson
import Data.ByteString                qualified as B
import Data.List.Extra                (maximumOn, minimumOn)
import Data.List.NonEmpty             (NonEmpty (..), nonEmpty)
import Data.List.NonEmpty             qualified as NonEmpty
import Data.Map.Monoidal              (MonoidalMap (..))
import Data.Map.Strict                (Map)
import Data.Map.Strict                qualified as Map
import Data.Maybe                     (mapMaybe)
import Data.Set                       qualified as Set
import Data.Time                      (UTCTime)
import Data.Time.Clock                (NominalDiffTime)
import GHC.Generics                   (Generic, Generically (..))
import Hyperion.Log                   qualified as Log
import Hyperion.OsPath                (OsPath, takeDirectory)
import Hyperion.OsString              (fromString, toString)
import Hyperion.Scheduler.StatKey     (FileStatKey, StatKey,
                                       TaskKeyFileInfo (..))
import Hyperion.Scheduler.Task.IsTask (IsTask (..), ResourceEstimates (..))
import Hyperion.Scheduler.Types       (Estimate, FileSize (..), MemorySize (..),
                                       Node, NumCPUs, modelEstimate,
                                       schedulingEstimate)
import Hyperion.Util                  (randomString)
import Prelude                        hiding (readFile, (^))
import Prelude qualified
import System.Directory.OsPath        (createDirectoryIfMissing, renameFile)
import System.File.OsPath             (readFile)

(^) :: Num a => a -> Int -> a
(^) = (Prelude.^)

-- | A record of task and information about when and how it ran
data TaskRecord a = MkTaskRecord
  { task          :: a
  , taskStart     :: UTCTime
  , taskRuntime   :: NominalDiffTime
  , taskMemory    :: Maybe MemorySize
  , taskNode      :: Node
  , taskNumCPUs   :: NumCPUs
  , taskFileSizes :: Map FileStatKey (NonEmpty FileSize)
  , taskEstimates :: TaskEstimates
  , taskStatKey   :: Maybe StatKey
    -- ^ Recorded rather than recomputed from 'task': a stat key is a reduced
    -- projection of the task, and the projection needs a typed key and its
    -- config, neither of which survives serialization. Without it a record read
    -- back from a file could not be grouped with its comparable siblings.
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, Functor)

-- | What the scheduler predicted for a task, recorded next to what the task
-- actually used, so the predictions can be judged after the run.
--
-- Estimates are deliberately kept out of 'TaskStats': that file is read back as
-- the input to scheduling, so a prediction stored there could later be consumed
-- as though it had been observed. A task record is only ever written.
data TaskEstimates = MkTaskEstimates
  { memory    :: Estimate MemorySize
  , runtime   :: Estimate NominalDiffTime
    -- ^ At the 'NumCPUs' the task was given, so it pairs with 'taskRuntime'.
  , fileSizes :: Map FileStatKey (Estimate FileSize)
    -- ^ Keyed as 'taskFileSizes' is, so predicted and actual sizes line up key
    -- by key. Covers inputs as well as outputs, while only output sizes are
    -- measured, so a record can predict a size it has no measurement for -- the
    -- task that produced that file has it.
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | The estimates a task was scheduled on, as of the given CPU allocation.
-- The runtime is evaluated from the very curve the scheduler used, so the two
-- cannot disagree.
taskEstimatesAt :: IsTask a => NumCPUs -> a -> TaskEstimates
taskEstimatesAt numCpus task = MkTaskEstimates
  { memory    = estimates.memory
  , runtime   = fmap ($ numCpus) estimates.runtime
  -- Files sharing a stat key are estimated alike, so the duplicates this
  -- discards are equal anyway.
  , fileSizes = Map.fromListWith max
      [ (statKey, info.fileSize)
      | info <- Set.toList $ Set.union (taskInputs task) (taskOutputs task)
      , Just statKey <- [info.fileStatKey]
      ]
  }
  where
    estimates = taskResourceEstimates task

-- | Measured resource usage over what was predicted for it, so a ratio above
-- one means the run used more than predicted -- the dangerous direction for
-- memory. 'Nothing' where nothing could be scored: no memory figure was
-- measured, or the prediction was zero and no ratio exists.
data Accuracy = MkAccuracy
  { memory  :: Maybe (Trials Double)
  , runtime :: Maybe (Trials Double)
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)
  deriving (Semigroup, Monoid) via (Generically Accuracy)

-- | How measured file sizes compare with what was predicted for them, grouped
-- by file stat key. Only sizes that were both predicted and measured are
-- scored, which in one record means its output files.
fileSizeAccuracy :: (forall x . Estimate x -> x) -> [TaskRecord a] -> Map FileStatKey (Trials Double)
fileSizeAccuracy predicted records = getMonoidalMap $ foldMap one records
  where
    one record = MonoidalMap $ Map.mapMaybe id $
      Map.intersectionWith score record.taskFileSizes record.taskEstimates.fileSizes
    score measured estimate = case realToFrac (predicted estimate) of
      prediction | prediction <= 0 -> Nothing
                 | otherwise -> Just $
                     toTrials $ NonEmpty.map ((/ prediction) . fromIntegral) measured

-- | How a run's measurements compare with the estimates it was actually
-- scheduled on, grouped by stat key. This scores the scheduling decisions,
-- including those made from recorded statistics.
estimateAccuracy :: [TaskRecord a] -> Map StatKey Accuracy
estimateAccuracy = accuracyBy (.taskStatKey) schedulingEstimate

-- | How a run's measurements compare with what the tasks' own models predicted,
-- ignoring any statistics that overrode them. This is what says whether a
-- 'Hyperion.Scheduler.StatKey.memoryEstimate' needs fixing, and it is
-- answerable for a task that has never run before.
modelAccuracy :: [TaskRecord a] -> Map StatKey Accuracy
modelAccuracy = accuracyBy (.taskStatKey) modelEstimate

-- | Scores records against one of their two estimates, grouped by whatever
-- identifies them. Records the grouping function rejects are left out.
--
-- Grouping by stat key is the precise choice and what the two functions above
-- use; grouping by a task's tag instead trades precision for a summary a person
-- can read, since a stat key prints as a whole JSON object.
accuracyBy
  :: Ord k
  => (TaskRecord a -> Maybe k)
  -> (forall x . Estimate x -> x)
  -> [TaskRecord a]
  -> Map k Accuracy
accuracyBy groupKey predicted records = getMonoidalMap $ foldMap one records
  where
    one record = case groupKey record of
      Nothing  -> mempty
      Just key -> MonoidalMap $ Map.singleton key MkAccuracy
        { memory = do
            measured <- record.taskMemory
            ratio (realToFrac measured) (realToFrac (predicted record.taskEstimates.memory))
        , runtime =
            ratio (realToFrac record.taskRuntime) (realToFrac (predicted record.taskEstimates.runtime))
        }
    ratio measured prediction
      | prediction <= 0 = Nothing
      | otherwise       = Just $ singleTrial (measured / prediction)

-- | Observations of one quantity, summarised so that they can be merged
-- without keeping the observations themselves.
--
-- 'mean' and 'variance' are 'Double' whatever is being measured, while 'min'
-- and 'max' keep the measured type. That is not just because an average of
-- integers is not an integer: a variance in bytes-squared overflows a 64-bit
-- 'Int' at 3 GB, and the merge below squares a difference of means before
-- dividing, which overflows at a 30 MB spread once a group holds a hundred
-- observations. 'min' and 'max' are only ever compared, never combined
-- arithmetically, so they can be exact.
data Trials a = MkTrials
  { mean      :: Double
  , min       :: a
  , max       :: a
  , variance  :: Double -- ^ biased variance, i.e. <x^2> - <x>^2
  , numTrials :: Int
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, NFData)

singleTrial :: Real a => a -> Trials a
singleTrial x = MkTrials (realToFrac x) x x 0 1

toTrials :: Real a => NonEmpty a -> Trials a
toTrials (x :| xs) = foldr (<>) (singleTrial x) $ map singleTrial xs

-- | Merging needs nothing of the measured type but 'Ord': the statistics that
-- require arithmetic are held as 'Double'.
instance Ord a => Semigroup (Trials a) where
  t1 <> t2 = MkTrials
    { mean      = (t1.mean*n1 + t2.mean*n2)/n12
    , min       = min t1.min t2.min
    , max       = max t1.max t2.max
    , variance  = (t1.mean-t2.mean)^2*n1*n2/(n12^2) + (n1*t1.variance + n2*t2.variance)/n12
    , numTrials = t1.numTrials + t2.numTrials
    }
    where
      n1 = fromIntegral t1.numTrials
      n2 = fromIntegral t2.numTrials
      n12 = n1 + n2

data TaskResources a = MkTaskResources
  { runtime :: a
  , memory  :: a
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, NFData, Functor)
  deriving (Semigroup, Monoid) via (Generically (TaskResources a))

newtype TaskResourceMap = MkTaskResourceMap (Map NumCPUs (TaskResources (Maybe (Trials Double))))
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON, NFData)
  deriving (Semigroup, Monoid) via (MonoidalMap NumCPUs (TaskResources (Maybe (Trials Double))))

taskResourceMapSingleton :: TaskRecord a -> TaskResourceMap
taskResourceMapSingleton record = MkTaskResourceMap $
  Map.singleton record.taskNumCPUs $ MkTaskResources
  { runtime = Just $ singleTrial (realToFrac record.taskRuntime)
  , memory  = fmap (singleTrial . realToFrac) record.taskMemory
  }

nonEmptyMemoryEntries :: TaskResourceMap -> Maybe (NonEmpty (NumCPUs, Trials Double))
nonEmptyMemoryEntries (MkTaskResourceMap m) = nonEmpty $ mapMaybe getMemory (Map.toList m)
  where
    getMemory (n, r) = case r.memory of
      Just mem -> Just (n, mem)
      Nothing  -> Nothing

nonEmptyRuntimeEntries :: TaskResourceMap -> Maybe (NonEmpty (NumCPUs, Trials Double))
nonEmptyRuntimeEntries (MkTaskResourceMap m) = nonEmpty $ mapMaybe getRuntime (Map.toList m)
  where
    getRuntime (n, r) = case r.runtime of
      Just t  -> Just (n,t)
      Nothing -> Nothing

maxMemory :: TaskResourceMap -> Maybe MemorySize
maxMemory = fmap (round . maximum . fmap ((.max) . snd)) . nonEmptyMemoryEntries

-- | Approximates speedup (defined as the un-normalized inverse time)
-- as a function of 'n', given the sample data. If n is to the left of
-- all of the sample points, we do a linear interpolation through the
-- origin and the leftmost point. If n is to the right of all the
-- sample points, we use Amdahl's law with some pre-specified p \in
-- [0,1]. Otherwise, we do linear interpolation in the interval where
-- n lives.
--
-- Edit: Actually, turning off Amdahl's law for now, since it seems to
-- lead to worse builds (expensive block_3d computations get WAY too
-- many cores).
-- p = 0.971371 -- Amdahl's law for blocks_3d
fitSpeedup :: Maybe Double -> NonEmpty (NumCPUs, Double) -> NumCPUs -> Double
fitSpeedup maybeAmdahlP speedups' = speedup
  where
    speedups = NonEmpty.toList speedups'

    (minN, minS) = minimumOn fst speedups
    (maxN, maxS) = maximumOn fst speedups

    speedup n
      | n <= minN = minS * (fromIntegral n / fromIntegral minN)
      | n >= maxN =
          case maybeAmdahlP of
            Just p -> amdahlFit p (maxN, maxS) n
            Nothing ->
              -- TODO: Currently this does a linear fit through the origin
              -- and the maximum point. Wouldn't it be better to do a
              -- linear interpolation through the first data point and the
              -- last point?
              maxS * (fromIntegral n / fromIntegral maxN)
      -- In this case, there must be at least two data points, with n
      -- lying between them
      | otherwise = go speedups
      where
        go ((n1,s1) : (n2,s2) : ss)
          | n >= n1 && n <= n2 =
            (fromIntegral (n-n1)*s2 + fromIntegral (n2-n)*s1) / fromIntegral (n2 - n1)
          | otherwise = go ((n2,s2) : ss)
        go _ = error $ concat
          [ "Couldn't find a pair of data points that n lies between"
          , "\nspeedups: ", show speedups
          , "\nn: ", show n
          ]

-- | Given a data point (n0,s0), where n0 = numThreads, and s0 is the
-- speedup (inverse time), and the proportion p in amdahl's law, find
-- the predicted speedup with n threads.
amdahlFit :: Double -> (NumCPUs, Double) -> NumCPUs -> Double
amdahlFit p (n0, s0) n = (1 - p + p/fromIntegral n0) / (1 - p + p/fromIntegral n) * s0

-- | Here, p is the parameter in Amdahl's law.
fitRuntime
  :: Maybe Double
  -> NonEmpty (NumCPUs, Trials Double)
  -> NumCPUs
  -> NominalDiffTime
fitRuntime p points = runtime
  where
    speedups = fmap (\(n,r) -> (n, 1 / r.mean)) points
    speedup = fitSpeedup p speedups
    runtime n = realToFrac (1 / speedup n)

-- | Here, p is the parameter in Amdahl's law, used when the number of
-- threads is larger than any given data points.
approxRuntime :: Maybe Double -> TaskResourceMap -> Maybe (NumCPUs -> NominalDiffTime)
approxRuntime p = fmap (fitRuntime p) . nonEmptyRuntimeEntries

-- | A map from StatKey to TaskResourceMap which describes the resource usage of the task.
newtype TaskStats = MkTaskStats (Map StatKey TaskResourceMap)
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON, NFData)
  deriving (Semigroup, Monoid) via (MonoidalMap StatKey TaskResourceMap)


-- TODO rename to recordToTaskAndFileStats
-- | Needs nothing of the task itself beyond what the record already states, so
-- it applies to records read back from a file as well as to freshly written
-- ones -- statistics can be rebuilt offline.
recordToTaskStats :: TaskRecord a -> TaskAndFileStats
recordToTaskStats record = MkTaskAndFileStats taskStats fileStats where
  -- A task with no stat key contributes no resource statistics: it performed
  -- no computation, so the only thing its runtime would measure is scheduler
  -- bookkeeping. Its file sizes (if any) are still recorded below.
  taskStats = MkTaskStats $ case record.taskStatKey of
    Nothing      -> Map.empty
    Just statKey -> Map.singleton statKey (taskResourceMapSingleton record)
  fileStats = MkFileStats $ Map.map toTrials record.taskFileSizes

-- | Sizes are exact: 'Trials' keeps its extremes in the measured type, so a
-- byte count never passes through a 'Double'.
newtype FileStats = MkFileStats (Map FileStatKey (Trials FileSize))
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON, NFData)
  deriving (Semigroup, Monoid) via (MonoidalMap FileStatKey (Trials FileSize))

data TaskAndFileStats = MkTaskAndFileStats TaskStats FileStats
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, NFData)
  deriving (Semigroup, Monoid) via Generically TaskAndFileStats

lookupMaxFileSize :: FileStatKey -> TaskAndFileStats -> Maybe FileSize
lookupMaxFileSize key (MkTaskAndFileStats _ (MkFileStats fileSizes)) =
  (.max) <$> Map.lookup key fileSizes

-- | Statistics are grouped by stat key, so lookup is an exact match: a key
-- that differs (for instance because an estimate-relevant config field
-- changed) simply misses, and the caller falls back to the analytic estimate.
lookupTaskStats :: StatKey -> TaskAndFileStats -> Maybe TaskResourceMap
lookupTaskStats statKey (MkTaskAndFileStats (MkTaskStats statsMap) _) =
  Map.lookup statKey statsMap

-- TODO: rename to writeTaskAndFileStats?
writeTaskStats :: MonadIO m => OsPath -> TaskAndFileStats -> m ()
writeTaskStats statFile stats = do
  Log.info "Writing TaskStats to file" statFile
  encodeJsonFileAtomic statFile stats

-- | Throws AesonException on parsing error.
throwDecodeFileStrict :: FromJSON a => OsPath -> IO a
throwDecodeFileStrict file = do
  bytes <- readFile file
  Aeson.throwDecodeStrict $ B.toStrict bytes

-- | Version of Aeson.eitherDecodeFileStrict
-- that also catches IOError
eitherReadFileStrict :: FromJSON a => OsPath -> IO (Either String a)
eitherReadFileStrict file = do
  (Right <$> throwDecodeFileStrict file)
    `catches`
    [ Handler (pure . Left . show @IOError)
    , Handler (pure . Left . show @AesonException)
    ]

-- | Read back records written by 'Hyperion.Scheduler.RunTasks.runTasks',
-- skipping (with a warning) any file that does not parse.
--
-- Read them as @'TaskRecord' 'Aeson.Value'@ unless the task type is known and
-- has a 'FromJSON' instance: a 'Hyperion.Scheduler.Task.WrappedTask.WrappedTask'
-- cannot have one, so the task itself usually stays uninterpreted. Everything
-- an estimate can be judged by -- the stat key, the estimates, the
-- measurements -- comes back typed regardless.
readTaskRecords :: (MonadIO m, FromJSON a) => [OsPath] -> m [TaskRecord a]
readTaskRecords files = concat <$> mapM readOne files
  where
    readOne file = do
      records <- liftIO $ eitherReadFileStrict file
      case records of
        Left e  -> Log.warn "Couldn't parse task records file" (file, e) >> pure []
        Right r -> pure r

-- TODO: rename to readTaskAndFileStats?
readTaskStats :: MonadIO m => [OsPath] -> m TaskAndFileStats
readTaskStats statFiles = do
  stats <- go statFiles mempty
  warnOnSingletonGroups stats
  pure stats
  where
    go [] acc = pure acc
    go (file : files) acc = do
      newStats_either <- liftIO $ eitherReadFileStrict file
      newStats <- case newStats_either of
        Left e  -> Log.warn "Couldn't parse stats file" (file, e) >> pure mempty
        Right s -> pure s
      newStats `deepseq` go files (acc <> newStats)

-- | A stat key exists to group comparable observations, so that a curve can be
-- fitted to them. If almost every group holds a single observation, the key is
-- probably carrying fields that do not affect resource usage, and should be
-- reduced -- no amount of further running will make such statistics useful.
warnOnSingletonGroups :: MonadIO m => TaskAndFileStats -> m ()
warnOnSingletonGroups (MkTaskAndFileStats (MkTaskStats taskStats) _)
  | total == 0            = pure ()
  | singletons * 2 <= total = pure ()
  | otherwise             = Log.warn
      "Most stat groups hold a single observation, so no runtime curve can be \
      \fitted to them. The stat keys are probably not reduced enough \
      \(groups with one observation, of total)"
      (singletons, total)
  where
    total = Map.size taskStats
    singletons = length $ filter isSingleton $ Map.elems taskStats
    isSingleton (MkTaskResourceMap m) = sum (map trialCount (Map.elems m)) <= (1 :: Int)
    trialCount r = maybe 0 (.numTrials) r.runtime

-- | Write a json representation of 'x' to a temporary file, and then
-- move the temporary file into place.
--
-- TODO: This is a copy of a function in SDPB. Export the function or
-- relocate it.
encodeJsonFileAtomic :: (MonadIO m, ToJSON a) => OsPath -> a -> m ()
encodeJsonFileAtomic path x = liftIO $ do
  salt <- randomString 6
  let tmpPath = path <> "_" <> fromString salt
  createDirectoryIfMissing True (takeDirectory path)
  Aeson.encodeFile (toString tmpPath) x
  renameFile tmpPath path
