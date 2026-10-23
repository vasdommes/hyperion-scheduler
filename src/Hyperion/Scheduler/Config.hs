module Hyperion.Scheduler.Config where

import Data.Time.Clock          (NominalDiffTime)
import Hyperion.OsPath          (OsPath)
import Hyperion.Scheduler.Types (FileSize, MemorySize)

-- TODO split into several (nested?) configs
data Config = MkConfig
  { nodeMemory           :: MemorySize -- ^ Total memory on the node
  , nodeLocalStorageSize :: FileSize
  , localStoragePath     :: OsPath -- ^ The node-local paths are those under it; none if empty
  , reportInterval       :: NominalDiffTime
  }
