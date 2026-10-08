{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StaticPointers             #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Hyperion.Scheduler.Types where

import Control.DeepSeq    (NFData)
import Data.Aeson         (FromJSON (..), ToJSON (..), (.:), (.=))
import Data.Aeson         qualified as Aeson
import Data.Binary        (Binary)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text          (Text)
import Data.Time.Clock    (NominalDiffTime)
import GHC.Generics       (Generic)
import Hyperion           (WorkerAddr (..))
import Hyperion           qualified as Hyp
import Hyperion.OsPath    (OsPath)
import Text.Printf        qualified as Printf

-- Nodes

-- TODO this conflicts with Hyperion.NumCPUs = NumCPUs Int
type NumCPUs = Hyp.NumCPUs

data Node = MkNode
  { memory           :: MemorySize -- ^ Total memory on the node
  , cpus             :: NumCPUs    -- ^ Number of CPUs on the node
  , localStoragePath :: OsPath   -- ^ Path to node-local storage. TODO: use Maybe OsPath?
  , localStorageSize :: FileSize   -- ^ Size of local storage on the node.
  , address          :: WorkerAddr -- ^ Address of the node
  } deriving (Eq, Ord, Show, Generic, Binary, ToJSON, FromJSON)

-- FileSize

type Bytes = Int

-- Wrapper for pretty printing
newtype FileSize = FileSize Bytes
  deriving stock    (Generic)
  deriving newtype  (Binary, Eq, Ord, Num, Enum, Real, Integral, ToJSON, FromJSON, NFData)

-- e.g.:
-- show (Filesize 1234567) == "1234567 (1.23 MB)"
-- show (Filesize 100) == "100 B"
instance Show FileSize where
  show (FileSize bytes) = prettyShowBytes KilobyteDecimal bytes

-- Wrapper for pretty printing. Same as FileSize, but with KB = 1024 B
-- TODO: use in the code
newtype MemorySize = MemorySize Bytes
  deriving stock    (Generic)
  deriving newtype  (Binary, Eq, Ord, Num, Enum, Real, Integral, ToJSON, FromJSON, NFData)

-- e.g.:
-- show (MemorySize 1234567) == "1234567 (1.18 MB)"
-- show (MemorySize 100) == "100 B"
instance Show MemorySize where
  show (MemorySize bytes) = prettyShowBytes KilobyteBinary bytes

data KilobyteSize = KilobyteDecimal | KilobyteBinary

-- Estimating runtime from memory. Here rather than in
-- 'Hyperion.Scheduler.Task.IsTask', so that 'Hyperion.Scheduler.StatKey' can
-- use them.

-- If you don't have a better way to estimate runtime of your task, try this
-- one.
-- It produces reasonably-looking times.
-- The constant 1.7e-6 originally came from our blocks_3d tests on Expanse.
memoryToCpuTimeApprox :: MemorySize -> NominalDiffTime
memoryToCpuTimeApprox mem = 1.7e-6 * fromIntegral mem

-- | Estimate runtime from memory when no task-specific estimate is available.
-- Zero CPUs is valid for scheduler-only tasks which do not perform computation.
defaultRuntimeEstimate :: MemorySize -> NumCPUs -> NominalDiffTime
defaultRuntimeEstimate mem numCpus
  | numCpus < 0 = error "defaultRuntimeEstimate: negative CPU count"
  | numCpus == 0 = 0
  | otherwise = memoryToCpuTimeApprox mem / fromIntegral numCpus

-- Estimates

-- | A figure the scheduler used, together with where it came from. Statistics
-- override a task's own model (see
-- 'Hyperion.Scheduler.Task.WrappedTask.decorateTaskWithStats'), and this keeps
-- the prediction that was overridden, so the model can be judged against the
-- measurement afterwards.
--
-- Provenance is per quantity, not per task: memory and runtime are looked up
-- independently, so a task can run on a measured runtime and its own memory
-- estimate.
data Estimate a
  = EstimatedByTask a
    -- ^ The task's own model, used as-is: no statistics matched its stat key.
  | MeasuredFromStats a a
    -- ^ The figure recovered from statistics, then the task's own prediction.
  deriving (Eq, Ord, Show, Generic, Functor)

-- | The figure the scheduler schedules on.
schedulingEstimate :: Estimate a -> a
schedulingEstimate (EstimatedByTask x)     = x
schedulingEstimate (MeasuredFromStats x _) = x

-- | What the task's own model predicted, whether or not it was scheduled on.
modelEstimate :: Estimate a -> a
modelEstimate (EstimatedByTask x)     = x
modelEstimate (MeasuredFromStats _ x) = x

isMeasuredFromStats :: Estimate a -> Bool
isMeasuredFromStats (EstimatedByTask _)     = False
isMeasuredFromStats (MeasuredFromStats _ _) = True

-- | Replace the figure with one measured from statistics, keeping the task's
-- own prediction. Idempotent, so overriding an already overridden estimate
-- cannot mistake an earlier measurement for the task's model.
overrideWithMeasured :: a -> Estimate a -> Estimate a
overrideWithMeasured measured e = MeasuredFromStats measured (modelEstimate e)

-- | A flat object with the same keys for every constructor, so that recorded
-- estimates are easy to compare with the measurements next to them. For
-- 'EstimatedByTask' the two figures are equal, and only one is read.
instance FromJSON a => FromJSON (Estimate a) where
  parseJSON = Aeson.withObject "Estimate" $ \o -> do
    scheduling <- o .: "scheduling"
    source     <- o .: "source"
    case source :: Text of
      "EstimatedByTask"   -> pure $ EstimatedByTask scheduling
      "MeasuredFromStats" -> MeasuredFromStats scheduling <$> o .: "model"
      _                   -> fail $ "Unknown estimate source: " <> show source

instance ToJSON a => ToJSON (Estimate a) where
  toJSON e = Aeson.object
    [ "scheduling" .= schedulingEstimate e
    , "model"      .= modelEstimate e
    , "source"     .= sourceName
    ]
    where
      sourceName :: Text
      sourceName = case e of
        EstimatedByTask _     -> "EstimatedByTask"
        MeasuredFromStats _ _ -> "MeasuredFromStats"

-- Helper function for FileSize and MemorySize
prettyShowBytes :: KilobyteSize -> Bytes -> String
prettyShowBytes kilobyteSize bytes = show bytes <> prettyBytes where
  prettyBytes = case maybeSmallUnits of
    -- Just add suffix "B"
    Nothing -> " B"
    -- Convert to KB/MB/GB
    Just smallUnits -> Printf.printf " (%.2f %s)" value unit where
      value :: Double = (fromIntegral bytes) / (fromIntegral unitBytes)
      (unitBytes, unit) = NonEmpty.last smallUnits
  maybeSmallUnits = NonEmpty.nonEmpty $ filter isSmallEnough units
  isSmallEnough (x, _) = x <= abs bytes
  units :: [(Bytes, String)] =
    [ (kilo, "KB")
    , (kilo * kilo, "MB")
    , (kilo * kilo * kilo, "GB")
    ]
  kilo = kilobyteToByte kilobyteSize
  kilobyteToByte KilobyteDecimal = 1000
  kilobyteToByte KilobyteBinary  = 1024
