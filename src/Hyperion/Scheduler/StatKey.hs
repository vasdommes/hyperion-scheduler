{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Hyperion.Scheduler.StatKey where

import Control.DeepSeq                 (NFData)
import Data.Aeson                      (FromJSON, FromJSONKey, ToJSON (..),
                                        ToJSONKey, (.=))
import Data.Aeson                      qualified as Aeson
import Data.Typeable                   (Typeable, typeOf)
import Data.Void                       (Void)
import GHC.Generics                    (Generic)
import Hyperion.Scheduler.FilePath     (VirtualFilePath (..))
import Hyperion.Scheduler.PathResolver (PathResolver (..))
import Hyperion.Scheduler.Types        (FileSize)

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

keyToJSONWithType :: (Typeable a, ToJSON a) => a -> Aeson.Value
keyToJSONWithType key = Aeson.object ["type" .= show (typeOf key), "key" .= Aeson.toJSON key]

mkStatKeyViaJSON :: (Typeable a, ToJSON a) => a -> StatKey
mkStatKeyViaJSON = MkStatKey . keyToJSONWithType

instance ToStatKey StatKey where
  toStatKey = id

instance ToStatKey ()


-- | Container for a file stat key.
newtype FileStatKey = MkFileStatKey Aeson.Value
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON, NFData)
  deriving anyclass (ToJSONKey, FromJSONKey)

class ToFileStatKey a where
  toFileStatKey :: a -> FileStatKey
  -- | A default implementation: FileStatKey = StatKey.
  default toFileStatKey :: ToStatKey a => a -> FileStatKey
  toFileStatKey key = MkFileStatKey value where
    (MkStatKey value) = toStatKey key

  toFileSize    :: a -> FileSize
  toFileSize _ = 0


-- OutKey k = Void means no files.
instance ToFileStatKey Void where
  toFileStatKey = \case {}

mkFileStatKeyViaJSON :: (ToJSON a, Typeable a) => a -> FileStatKey
mkFileStatKeyViaJSON = MkFileStatKey . keyToJSONWithType

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
