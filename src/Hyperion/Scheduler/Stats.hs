{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StaticPointers        #-}
{-# LANGUAGE TypeFamilies          #-}

module Hyperion.Scheduler.Stats
  ( TaskAndFileStats (..)
  , TaskRecord (..)
  , TaskStats (..)
  , FileStats (..)
  , StatKey (..)
  , ToStatKey (..)
  , Trials (..)
  , mkStatKeyViaJSON
  , recordToTaskStats
  , approxRuntime
  , maxMemory
  , lookupTaskStats
  , lookupMaxFileSize
  , readTaskStats
  , writeTaskStats
  , encodeJsonFileAtomic
  ) where

import Control.DeepSeq                    (NFData, deepseq)
import Control.Monad.Catch                (Handler (..), catches)
import Control.Monad.IO.Class             (MonadIO, liftIO)
import Data.Aeson                         (AesonException, FromJSON,
                                           FromJSONKey, ToJSON, ToJSONKey, (.=))
import Data.Aeson                         qualified as Aeson
import Data.ByteString                    qualified as B
import Data.List.Extra                    (maximumOn, minimumOn)
import Data.List.NonEmpty                 (NonEmpty (..), nonEmpty)
import Data.List.NonEmpty                 qualified as NonEmpty
import Data.Map.Monoidal                  (MonoidalMap (..))
import Data.Map.Strict                    (Map)
import Data.Map.Strict                    qualified as Map
import Data.Maybe                         (mapMaybe)
import Data.Time (UTCTime)
import Data.Time.Clock                    (NominalDiffTime)
import Data.Typeable                      (Typeable, typeOf)
import GHC.Generics                       (Generic, Generically (..))
import Hyperion.Log                       qualified as Log
import Hyperion.OsPath                    (OsPath, takeDirectory)
import Hyperion.OsString                  (fromString, toString)
import Hyperion.Scheduler.TaskKeyFileInfo (FileStatKey)
import Hyperion.Scheduler.Types (FileSize (..), MemorySize (..), Node,
                                           NumCPUs)
import Hyperion.Util                      (randomString)
import Prelude                            hiding (readFile, (^))
import Prelude qualified
import System.Directory.OsPath            (createDirectoryIfMissing, renameFile)
import System.File.OsPath                 (readFile)

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
  } deriving (Eq, Ord, Show, Generic, ToJSON, Functor)

-- TODO: Maybe we don't need all of these quantities
data Trials a = MkTrials
  { mean      :: a
  , min       :: a
  , max       :: a
  , variance  :: a -- ^ biased variance, i.e. sqrt (<x^2> - <x>^2)
  , numTrials :: Int
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, NFData)

singleTrial :: Num a => a -> Trials a
singleTrial x = MkTrials x x x 0 1

toTrials :: (Num a, Semigroup (Trials a)) => NonEmpty a -> Trials a
toTrials (x :| xs) = foldr (<>) (singleTrial x) $ map singleTrial xs

instance (Floating a, Ord a) => Semigroup (Trials a) where
  t1 <> t2 = MkTrials
    { mean      = (t1.mean*n1 + t2.mean*n2)/n12
    , min       = min t1.min t2.min
    , max       = max t1.max t2.max
    , variance  = sqrt $ (t1.mean-t2.mean)^2*n1*n2/(n12^2) + (n1*t1.variance^2 + n2*t2.variance^2)/n12
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

newtype StatKey = MkStatKey Aeson.Value
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON, NFData)
  deriving anyclass (ToJSONKey, FromJSONKey)

class ToStatKey a where
  toStatKey :: a -> StatKey
  -- | A default implementation for the case where we wish to retain
  -- all the information about a task in the StatKey.
  default toStatKey :: (Typeable a, ToJSON a) => a -> StatKey
  toStatKey = mkStatKeyViaJSON

mkStatKeyViaJSON :: (Typeable a, ToJSON a) => a -> StatKey
mkStatKeyViaJSON key = MkStatKey $
  Aeson.object ["type" .= show (typeOf key), "key" .= Aeson.toJSON key]

instance ToStatKey StatKey where
  toStatKey = id

instance ToStatKey ()

-- TODO rename to recordToTaskAndFileStats
recordToTaskStats :: ToStatKey a => TaskRecord a -> TaskAndFileStats
recordToTaskStats record = MkTaskAndFileStats taskStats fileStats where
  taskStats = MkTaskStats $
    Map.singleton (toStatKey record.task)
    (taskResourceMapSingleton record)
  fileStats = MkFileStats $ Map.map toTrials' $ record.taskFileSizes
  fromIntegral' = NonEmpty.map fromIntegral
  toTrials' = toTrials . fromIntegral'

-- We need Double instead of Int because of Trials.variance
-- TODO: variance is never used, shall we remove it and switch to Int?
newtype FileStats = MkFileStats (Map FileStatKey (Trials Double))
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON, NFData)
  deriving (Semigroup, Monoid) via (MonoidalMap FileStatKey (Trials Double))

data TaskAndFileStats = MkTaskAndFileStats TaskStats FileStats
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, NFData)
  deriving (Semigroup, Monoid) via Generically TaskAndFileStats

lookupMaxFileSize :: FileStatKey -> TaskAndFileStats -> Maybe FileSize
lookupMaxFileSize key (MkTaskAndFileStats _ (MkFileStats fileSizes)) =
  FileSize <$> ceiling <$> (.max) <$> Map.lookup key fileSizes

lookupTaskStats :: ToStatKey a => a -> TaskAndFileStats -> Maybe TaskResourceMap
lookupTaskStats task (MkTaskAndFileStats (MkTaskStats statsMap) _) =
  Map.lookup (toStatKey task) statsMap

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

-- TODO: rename to readTaskAndFileStats?
readTaskStats :: MonadIO m => [OsPath] -> m TaskAndFileStats
readTaskStats statFiles = go statFiles mempty
  where
    go [] acc = pure acc
    go (file : files) acc = do
      newStats_either <- liftIO $ eitherReadFileStrict file
      newStats <- case newStats_either of
        Left e  -> Log.warn "Couldn't parse stats file" (file, e) >> pure mempty
        Right s -> pure s
      newStats `deepseq` go files (acc <> newStats)

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
