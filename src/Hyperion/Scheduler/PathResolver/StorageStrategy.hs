{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StaticPointers      #-}

-- | An optional kind of path resolver: a storage strategy puts each key's
-- file in node-local, global or persistent storage, or in a directory it
-- chooses, and 'ToRelativePath' gives its path there. A project can instead
-- write its own 'PathResolver'.
module Hyperion.Scheduler.PathResolver.StorageStrategy
  ( StorageType (..)
  , ToStorageType (..)
  , ToRelativePath (..)
  , PathResolverWithStrategy (..)
  , storageDir
  ) where

import Data.Aeson                      (FromJSON, ToJSON)
import Data.Binary                     (Binary)
import Data.Typeable                   (Typeable)
import GHC.Generics                    (Generic)
import Hyperion                        (Dict (..), Static (..), cAp)
import Hyperion.OsPath                 (OsPath, (</>))
import Hyperion.Scheduler.PathResolver (PathResolver (..))

-- | Where a file goes.
data StorageType
  = Local      -- ^ Node-local storage, removed with the job (see 'Hyperion.Scheduler.Config.localStoragePath')
  | Global     -- ^ Storage seen by all nodes, for the files of one computation
  | Persistent -- ^ Storage seen by all nodes, for files kept across computations
  | Custom OsPath
  -- ^ A directory the strategy chooses, e.g. to share files between
  -- computations. Caveats:
  --
  -- * Sharing is safe only if a file's relative path determines its contents.
  --   Otherwise two tasks with different outputs and the same path overwrite
  --   or wrongly reuse each other's file.
  --
  -- * The directory is outside the resolver's directories, so code that
  --   cleans those up does not remove it.
  --
  -- * Its files are node-local if it is under the node-local storage path,
  --   as for any path.
  deriving (Eq, Ord, Show, Generic, Binary, ToJSON, FromJSON)

instance Static (Binary StorageType) where
  closureDict = static Dict

-- | The storage that strategy @s@ chooses for key @k@.
class ToStorageType s k where
  toStorageType :: s -> k -> StorageType

-- | The path of a key's file, relative to its storage directory.
class ToRelativePath k where
  toRelativePath :: k -> OsPath

-- | Resolves a key to 'toRelativePath' in the directory of the storage that
-- the strategy chooses for it.
data PathResolverWithStrategy s = MkPathResolverWithStrategy
  { localDir        :: OsPath
  , globalDir       :: OsPath
  , persistentDir   :: OsPath
  , storageStrategy :: s
  }
  deriving (Eq, Ord, Show, Generic, Binary, ToJSON, FromJSON)

storageDir :: PathResolverWithStrategy s -> StorageType -> OsPath
storageDir resolver = \case
  Local      -> resolver.localDir
  Global     -> resolver.globalDir
  Persistent -> resolver.persistentDir
  Custom dir -> dir

instance (ToStorageType s k, ToRelativePath k) => PathResolver (PathResolverWithStrategy s) k where
  resolvePath resolver key =
    storageDir resolver (toStorageType resolver.storageStrategy key) </> toRelativePath key

instance
  ( Typeable s
  , Typeable k
  , Static (ToStorageType s k)
  , Static (ToRelativePath k)
  ) => Static (PathResolver (PathResolverWithStrategy s) k) where
    closureDict = static (\Dict -> Dict) `cAp` closureDict @(ToStorageType s k, ToRelativePath k)

instance (Typeable s, Static (Binary s)) => Static (Binary (PathResolverWithStrategy s)) where
  closureDict = static (\Dict -> Dict) `cAp` closureDict @(Binary s)

instance (Typeable s, Static (Ord s)) => Static (Ord (PathResolverWithStrategy s)) where
  closureDict = static (\Dict -> Dict) `cAp` closureDict @(Ord s)

instance (Typeable s, Static (Show s)) => Static (Show (PathResolverWithStrategy s)) where
  closureDict = static (\Dict -> Dict) `cAp` closureDict @(Show s)

instance (Typeable s, Static (ToJSON s)) => Static (ToJSON (PathResolverWithStrategy s)) where
  closureDict = static (\Dict -> Dict) `cAp` closureDict @(ToJSON s)
