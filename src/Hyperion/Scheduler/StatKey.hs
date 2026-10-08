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
import Data.Text                       (Text)
import Data.Time.Clock                 (NominalDiffTime)
import Data.Typeable                   (Typeable)
import Data.Void                       (Void, absurd)
import GHC.Generics                    (Generic)
import Hyperion.Scheduler.FilePath     (VirtualFilePath (..))
import Hyperion.Scheduler.PathResolver (PathResolver (..))
import Hyperion.Scheduler.Types        (Estimate (..), FileSize, MemorySize,
                                        NumCPUs, defaultRuntimeEstimate)
import Hyperion.Scheduler.Util         (qualifiedTypeRepText)

newtype StatKey = MkStatKey Aeson.Value
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON, NFData)
  deriving anyclass (ToJSONKey, FromJSONKey)

-- | A stat key is the identity under which a task's resource usage is
-- recorded, and the only input to its estimates, so an estimate can always be
-- checked against the statistics recorded under its own key.
--
-- Make it a /reduced/ projection of the task key: drop or coarsen the fields
-- that do not affect resource usage, so that tasks differing only in those
-- share statistics. Project only the estimate-relevant parts of the config (a
-- version or variant tag), never a filesystem path.
class (Typeable a, ToJSON a, FromJSON a) => IsStatKey a where
  -- | Estimated memory in bytes: the resident set of the whole worker process
  -- while this task runs, not only what the task allocates. Each concurrent
  -- task is its own worker, so include the executable's own footprint (tens
  -- of megabytes); statistics measure the same thing.
  memoryEstimate :: a -> MemorySize

  -- | Estimated runtime in seconds, as a function of 'NumCPUs'.
  runtimeEstimate :: a -> NumCPUs -> NominalDiffTime
  runtimeEstimate = defaultRuntimeEstimate . memoryEstimate

  -- | The tag written into stat files to identify this key's type: by default
  -- its name with its module, so that types of the same name do not share
  -- statistics. It is part of the on-disk format, so override it if you
  -- rename the type or move the module and want previously recorded
  -- statistics to keep matching.
  statKeyTypeName :: Text
  default statKeyTypeName :: Text
  statKeyTypeName = qualifiedTypeRepText @a

-- | The identity under which the size of an /output file/ is recorded. It
-- takes no config: a file's size is a property of what was computed.
-- 'FromJSON' is required only by 'decodeFileStatKey': a file stat key may be
-- the output key itself, which need not be parseable.
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
  fileStatKeyTypeName = qualifiedTypeRepText @a

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
encodeStatKey = MkStatKey . encodeTagged (statKeyTypeName @a)

-- | Inverse of 'encodeStatKey'. Returns 'Nothing' if the tag names a different
-- type, or the payload does not parse.
decodeStatKey :: forall a . IsStatKey a => StatKey -> Maybe a
decodeStatKey (MkStatKey value) = decodeTagged (statKeyTypeName @a) value

-- | 'encodeStatKey' for file stat keys.
encodeFileStatKey :: forall a . IsFileStatKey a => a -> FileStatKey
encodeFileStatKey = MkFileStatKey . encodeTagged (fileStatKeyTypeName @a)

-- | 'decodeStatKey' for file stat keys.
decodeFileStatKey
  :: forall a . (IsFileStatKey a, FromJSON a) => FileStatKey -> Maybe a
decodeFileStatKey (MkFileStatKey value) =
  decodeTagged (fileStatKeyTypeName @a) value

-- | A value with its type tag.
encodeTagged :: ToJSON a => Text -> a -> Aeson.Value
encodeTagged tag key = Aeson.object ["type" .= tag, "key" .= key]

-- | Inverse of 'encodeTagged', 'Nothing' for another tag.
decodeTagged :: FromJSON a => Text -> Aeson.Value -> Maybe a
decodeTagged tag = Aeson.parseMaybe $ Aeson.withObject "tagged key" $ \o -> do
  tag' <- o .: "type"
  guard (tag' == tag)
  o .: "key"

-- | Container for a file stat key.
newtype FileStatKey = MkFileStatKey Aeson.Value
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON, NFData)
  deriving anyclass (ToJSONKey, FromJSONKey)

-- | Projects an output key onto the key under which its file's size is
-- recorded and estimated, so that the recorded identity and the estimate
-- cannot drift apart. Defaults to 'Void': no file statistics, and a size of
-- zero, which only under-counts node-local storage in @canHandleTask@.
-- Prefer a /reduced/ projection: an unreduced key puts every file in a group
-- of its own, which cannot predict a new file.
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
  , fileSize    :: Estimate FileSize
    -- ^ Carries its provenance for the same reason a task's memory estimate
    -- does: 'Hyperion.Scheduler.Task.WrappedTask.decorateTaskWithStats'
    -- replaces it with a recorded size, and what the key itself declared is
    -- then the only way to tell whether 'fileSizeEstimate' is any good.
  }
  deriving (Generic, Eq, Ord, Show)

type ToTaskKeyFileInfo r a = (PathResolver r a, ToFileStatKey a)

toTaskKeyFileInfo
  :: ToTaskKeyFileInfo r a
  => r -> a -> TaskKeyFileInfo
toTaskKeyFileInfo resolver key = MkTaskKeyFileInfo
  { fileStatKey      = toFileStatKey key
  , path             = VirtualFilePath $ resolvePath resolver key
  , fileSize         = EstimatedByTask $ toFileSize key
  }
