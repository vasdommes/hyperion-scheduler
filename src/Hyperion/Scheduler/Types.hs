{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StaticPointers             #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Hyperion.Scheduler.Types where

import Control.DeepSeq    (NFData)
import Data.Aeson         (FromJSON, ToJSON)
import Data.Binary        (Binary)
import Data.List.NonEmpty qualified as NonEmpty
import GHC.Generics       (Generic)
import Hyperion           (WorkerAddr (..))
import Hyperion.OsPath    (OsPath)
import Text.Printf        qualified as Printf

-- Nodes

-- TODO this conflicts with Hyperion.NumCPUs = NumCPUs Int
type NumCPUs = Int

data Node = MkNode
  { memory           :: MemorySize -- ^ Total memory on the node
  , cpus             :: NumCPUs    -- ^ Number of CPUs on the node
  , localStoragePath :: OsPath   -- ^ Path to node-local storage. TODO: use Maybe OsPath?
  , localStorageSize :: FileSize   -- ^ Size of local storage on the node.
  , address          :: WorkerAddr -- ^ Address of the node
  } deriving (Eq, Ord, Show, Generic, Binary, ToJSON)

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
