module Hyperion.Scheduler.Config where

import Data.Time.Clock          (NominalDiffTime)
import Hyperion.OsPath          (OsPath)
import Hyperion.Scheduler.Types (FileSize, MemorySize)

-- TODO split into several (nested?) configs
data Config = MkConfig
  { nodeMemory           :: MemorySize -- ^ Total memory on the node
  , nodeLocalStorageSize :: FileSize
  , localStoragePath     :: OsPath
  , isLocalPath          :: OsPath -> Bool
  , reportInterval       :: NominalDiffTime
  }

-- | Paths the scheduler knows to be absent without the filesystem, for
-- 'Hyperion.Scheduler.TaskFiles.runMemoizedTaskFilesWith' when building a task
-- map. A node-local path never exists: the file service knows nothing of local
-- files from before the run, so it could not hand one to a consumer, and the
-- filesystem would only show the building node's local storage.
schedulerAbsentPaths :: Config -> OsPath -> Bool
schedulerAbsentPaths MkConfig { isLocalPath = isLocal } = isLocal
