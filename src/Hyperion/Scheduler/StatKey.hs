{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE DerivingStrategies      #-}
{-# LANGUAGE DuplicateRecordFields   #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE NoFieldSelectors        #-}
{-# LANGUAGE OverloadedRecordDot     #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Hyperion.Scheduler.StatKey where

import Control.DeepSeq                 (NFData)
import Control.Monad                   (guard)
import Data.Aeson                      (FromJSON, FromJSONKey, ToJSON (..),
                                        ToJSONKey, (.:), (.=))
import Data.Aeson                      qualified as Aeson
import Data.Aeson.Types                qualified as Aeson
import Data.Proxy                      (Proxy (..))
import Data.Text                       (Text)
import Data.Text                       qualified as Text
import Data.Time.Clock                 (NominalDiffTime)
import Data.Typeable                   (Typeable, typeRep)
import Data.Void                       (Void, absurd)
import GHC.Generics                    (Generic)
import Hyperion.Scheduler.FilePath     (VirtualFilePath (..))
import Hyperion.Scheduler.PathResolver (PathResolver (..))
import Hyperion.Scheduler.Types        (FileSize, MemorySize, NumCPUs,
                                        defaultRuntimeEstimate)

newtype StatKey = MkStatKey Aeson.Value
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON, NFData)
  deriving anyclass (ToJSONKey, FromJSONKey)

-- | A stat key is the identity under which a task's resource usage is
-- recorded, and the /only/ input to that task's estimates. If an estimate
-- needs some piece of information, that information belongs in the stat key.
--
-- Two consequences are deliberate:
--
--   * Scheduling and validation use the same function, so an estimate can
--     always be checked against the statistics recorded under its own key.
--   * A stat key should be a /reduced/ projection of a task key: fields that
--     do not affect resource usage (a numeric coupling, a file path) should be
--     dropped or coarsened, so that tasks differing only in those fields share
--     statistics.
--
-- A task's config ('Hyperion.Scheduler.Task.Task.TaskConfig') says /how/ to
-- compute, so it may legitimately affect estimates. Project the parts that do
-- (a version or variant tag) into the stat key when building it; never the
-- whole config, and never a filesystem path -- a path changes when nothing
-- about the computation changed, which would split recorded history for no
-- reason.
class (Typeable a, ToJSON a, FromJSON a) => IsStatKey a where
  -- | Estimated memory in bytes.
  memoryEstimate :: a -> MemorySize

  -- | Estimated runtime in seconds, as a function of 'NumCPUs'.
  runtimeEstimate :: a -> NumCPUs -> NominalDiffTime
  runtimeEstimate = defaultRuntimeEstimate . memoryEstimate

  -- | The tag written into stat files to identify this key's type. It is part
  -- of the on-disk format, so override it if you rename the type or move the
  -- module and want previously recorded statistics to keep matching.
  statKeyTypeName :: Text
  default statKeyTypeName :: Text
  statKeyTypeName = Text.pack $ show $ typeRep (Proxy @a)

-- | The identity under which the size of an /output file/ is recorded.
--
-- Unlike 'IsStatKey' this takes no config: a file's size is a property of the
-- result (what was computed), and the config only describes how to compute it.
-- NB: 'FromJSON' is required by 'decodeFileStatKey' rather than by the class,
-- unlike 'IsStatKey'. A stat key is always a purpose-built reduced record, so
-- demanding round-trippability of it is cheap; a file stat key defaults to the
-- output key itself, and those are not generally parseable.
class (Typeable a, ToJSON a) => IsFileStatKey a where
  -- | Estimated size of the file, in bytes.
  --
  -- Defaults to zero, i.e. unknown. That only under-counts node-local storage
  -- in 'canHandleTask' until a real size has been measured and recorded.
  fileSizeEstimate :: a -> FileSize
  fileSizeEstimate _ = 0

  -- | See 'statKeyTypeName'.
  fileStatKeyTypeName :: Text
  default fileStatKeyTypeName :: Text
  fileStatKeyTypeName = Text.pack $ show $ typeRep (Proxy @a)

instance IsFileStatKey Void where
  fileSizeEstimate = absurd

-- | Tasks that are never scheduled by estimate -- placeholders, which are
-- replaced before the map is run, and no-ops, which perform no computation --
-- declare @type instance StatKeyOf MyKey = Void@ and return 'Nothing' from
-- @toStatKey@. There is no value to estimate from, hence 'absurd'.
--
-- Use that rather than an estimate of @0@ on a task that really does run: a
-- zero estimate is indistinguishable from a real one, and schedules the task
-- as though it were free.
instance IsStatKey Void where
  memoryEstimate = absurd

-- | Serialize a stat key together with its type tag. This is the form stored
-- in stat files and used as the grouping key when statistics are merged.
encodeStatKey :: forall a . IsStatKey a => a -> StatKey
encodeStatKey key = MkStatKey $ Aeson.object
  [ "type" .= statKeyTypeName @a
  , "key"  .= Aeson.toJSON key
  ]

-- | Inverse of 'encodeStatKey'. Returns 'Nothing' if the tag names a different
-- type, or the payload does not parse.
decodeStatKey :: forall a . IsStatKey a => StatKey -> Maybe a
decodeStatKey (MkStatKey value) = Aeson.parseMaybe parse value
  where
    parse = Aeson.withObject "StatKey" $ \o -> do
      tag <- o .: "type"
      guard (tag == statKeyTypeName @a)
      o .: "key"

-- | 'encodeStatKey' for file stat keys.
encodeFileStatKey :: forall a . IsFileStatKey a => a -> FileStatKey
encodeFileStatKey key = MkFileStatKey $ Aeson.object
  [ "type" .= fileStatKeyTypeName @a
  , "key"  .= Aeson.toJSON key
  ]

-- | 'decodeStatKey' for file stat keys.
decodeFileStatKey :: forall a . (IsFileStatKey a, FromJSON a) => FileStatKey -> Maybe a
decodeFileStatKey (MkFileStatKey value) = Aeson.parseMaybe parse value
  where
    parse = Aeson.withObject "FileStatKey" $ \o -> do
      tag <- o .: "type"
      guard (tag == fileStatKeyTypeName @a)
      o .: "key"

-- | Container for a file stat key.
newtype FileStatKey = MkFileStatKey Aeson.Value
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON, NFData)
  deriving anyclass (ToJSONKey, FromJSONKey)

-- | Projects an output key onto the key under which its file's size is
-- recorded and estimated.
--
-- Both the recorded identity ('toFileStatKey') and the estimate
-- ('toFileSize') are derived from this single projection, so they cannot
-- drift apart -- the estimate can never read a field that the recorded
-- identity dropped, which would make the two incomparable.
--
-- Defaults to 'Void', i.e. no file statistics: the file's size is neither
-- recorded nor looked up, and is estimated as zero. This mirrors the default
-- 'Hyperion.Scheduler.Task.Task.StatKeyOf' on the task side -- nothing is
-- collected until a task asks for it. A zero size only under-counts
-- node-local storage in @canHandleTask@, which matters when that storage is
-- the binding constraint.
--
-- When declaring one, prefer a /reduced/ projection where the file size
-- depends on only part of the key: file statistics are grouped by this type,
-- so an unreduced key yields one observation per file, which can report the
-- size of a file already produced but cannot predict a new one.
class IsFileStatKey (FileStatKeyOf a) => ToFileStatKey a where
  type FileStatKeyOf a
  type FileStatKeyOf a = Void

  fileStatKeyOf :: a -> Maybe (FileStatKeyOf a)
  fileStatKeyOf _ = Nothing

-- | How an output key is identified in file statistics, if at all.
toFileStatKey :: ToFileStatKey a => a -> Maybe FileStatKey
toFileStatKey = fmap encodeFileStatKey . fileStatKeyOf

-- | How big the output file is expected to be. Zero when the key declares no
-- file stat key, i.e. when the size is simply unknown.
toFileSize :: ToFileStatKey a => a -> FileSize
toFileSize = maybe 0 fileSizeEstimate . fileStatKeyOf

-- OutKey k = Void means no files.
instance ToFileStatKey Void

data TaskKeyFileInfo = MkTaskKeyFileInfo
  { fileStatKey :: Maybe FileStatKey
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
