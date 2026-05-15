{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE StaticPointers      #-}
{-# LANGUAGE TypeApplications    #-}

module Hyperion.Scheduler.FilePath where

import Data.Binary               (Binary)
import GHC.Generics              (Generic)
import Hyperion                  (Dict (..), Static, WorkerAddr (..),
                                  closureDict)
import Hyperion.OsPath           (OsPath)
import Hyperion.OsString         (OsString, toString)
import Hyperion.Scheduler.Config (Config (..))

-- | VirtualFilePath is simply a file path that does not know about cluster specifics,
--   i.e. whether it belongs to a local storage on some node or to a distributed filesystem (Lustre).
-- This type is used in TaskInfo.
-- Scheduler will transform it into CLusterFilePath.
-- TODO use smth better than FilePath
--   (OsPath is the new standard: https://hackage.haskell.org/package/filepath-1.5.4.0/docs/System-OsPath.html)
data VirtualFilePath = VirtualFilePath OsPath
  deriving (Eq, Ord, Show, Binary, Generic)

instance Static (Binary VirtualFilePath) where
  closureDict = static Dict

-- | NodeLocalFilePath: absolute path to a file at the local storage on some node.
-- GlobalFilePath: absolute path to a file stored at distributed filesystem (e.g. Lustre) available from any node.
data ClusterFilePath = NodeLocalFilePath WorkerAddr OsPath | GlobalFilePath OsPath
  deriving (Eq, Ord, Generic, Binary)

instance Static (Binary ClusterFilePath) where
  closureDict = static Dict

toVirtualFilePath :: ClusterFilePath -> VirtualFilePath
toVirtualFilePath (GlobalFilePath p)      = VirtualFilePath p
toVirtualFilePath (NodeLocalFilePath _ p) = VirtualFilePath p

instance Show ClusterFilePath where
  show (GlobalFilePath p) = toString $ "GlobalFilePath " <> p
  show (NodeLocalFilePath node p) = toString $ "NodeLocalFilePath "<> getAddr node <> ":" <> p

toClusterFilePath :: Config -> WorkerAddr -> VirtualFilePath -> ClusterFilePath
toClusterFilePath config node vp@(VirtualFilePath p)
  | isNodeLocal config vp = NodeLocalFilePath node p
  | otherwise = GlobalFilePath p

-- | Check if the file path is node-local or global.
isNodeLocal :: Config -> VirtualFilePath -> Bool
isNodeLocal config (VirtualFilePath path) = config.isLocalPath path

getAddr :: WorkerAddr -> OsString
getAddr (LocalHost a)  = a
getAddr (RemoteAddr a) = a
