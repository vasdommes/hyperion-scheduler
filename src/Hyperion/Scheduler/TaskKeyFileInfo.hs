{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Hyperion.Scheduler.TaskKeyFileInfo where

import Control.DeepSeq                 (NFData)
import Data.Aeson                      (FromJSON, FromJSONKey, ToJSON (..),
                                        ToJSONKey)
import Data.Aeson                      qualified as Aeson
import GHC.Generics                    (Generic)
import Hyperion.Scheduler.FilePath     (VirtualFilePath (..))
import Hyperion.Scheduler.PathResolver (PathResolver (..))
import Hyperion.Scheduler.Types        (FileSize)

-- | Container for a file stat key.
newtype FileStatKey = MkFileStatKey Aeson.Value
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON, NFData)
  deriving anyclass (ToJSONKey, FromJSONKey)

-- TODO remove?
class ToFileStatKey a where
  toFileStatKey :: a -> FileStatKey
  toFileSize    :: a -> FileSize

  -- | A default implementation for the case where we wish to retain
  -- all the information about a task in the FileStatKey.
  default toFileStatKey :: ToJSON a => a -> FileStatKey
  toFileStatKey = mkFileStatKeyViaJSON

mkFileStatKeyViaJSON :: ToJSON a => a -> FileStatKey
mkFileStatKeyViaJSON = MkFileStatKey . Aeson.toJSON

data TaskKeyFileInfo = MkTaskKeyFileInfo
  { fileStatKey :: FileStatKey
  , path        :: VirtualFilePath
  , fileSize    :: FileSize
  }
  deriving (Generic, Eq, Ord, Show)

type ToTaskKeyFileInfo r a = (PathResolver r a, ToFileStatKey a)

toTaskKeyFileInfo
  :: ToTaskKeyFileInfo r a
  => r -> a -> TaskKeyFileInfo
toTaskKeyFileInfo resolver key = MkTaskKeyFileInfo
  { fileStatKey      = toFileStatKey key
  , path             = VirtualFilePath $ resolvePath resolver key
  , fileSize         = toFileSize key
  }
