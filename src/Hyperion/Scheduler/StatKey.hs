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
                                        NumCPUs, defaultRuntimeEstimate,
                                        schedulingEstimate)
import Hyperion.Scheduler.Util         (typeRepText)

newtype StatKey = MkStatKey Aeson.Value
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON, NFData)
  deriving anyclass (ToJSONKey, FromJSONKey)

-- | What every task knows about one of its input files, whatever the key type.
data InputFileSummary = MkInputFileSummary
  { fileStatKey :: Maybe FileStatKey
  , size        :: FileSize
  }

-- | A summary of a task's input files, built one file at a time without the
-- dependency key types. The logic of a stock summary lives here, once.
class Monoid s => FromInputFiles s where
  fromInputFile :: InputFileSummary -> s

-- | The estimates ignore the inputs.
instance FromInputFiles () where
  fromInputFile _ = ()

-- | Two summaries of the same files.
instance (FromInputFiles a, FromInputFiles b) => FromInputFiles (a, b) where
  fromInputFile i = (fromInputFile i, fromInputFile i)

-- | The size of the largest input file, 0 if there are none.
newtype MaxInputFileSize = MkMaxInputFileSize FileSize
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

instance Semigroup MaxInputFileSize where
  MkMaxInputFileSize x <> MkMaxInputFileSize y = MkMaxInputFileSize (max x y)

instance Monoid MaxInputFileSize where
  mempty = MkMaxInputFileSize 0

instance FromInputFiles MaxInputFileSize where
  fromInputFile i = MkMaxInputFileSize i.size

-- | The total size of the input files.
newtype TotalInputFileSize = MkTotalInputFileSize FileSize
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

instance Semigroup TotalInputFileSize where
  MkTotalInputFileSize x <> MkTotalInputFileSize y = MkTotalInputFileSize (x + y)

instance Monoid TotalInputFileSize where
  mempty = MkTotalInputFileSize 0

instance FromInputFiles TotalInputFileSize where
  fromInputFile i = MkTotalInputFileSize i.size

-- | The sizes of the input files, sorted (a multiset).
newtype InputFileSizes = MkInputFileSizes [FileSize]
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

instance Semigroup InputFileSizes where
  MkInputFileSizes x <> MkInputFileSizes y = MkInputFileSizes (mergeSorted x y)

instance Monoid InputFileSizes where
  mempty = MkInputFileSizes []

instance FromInputFiles InputFileSizes where
  fromInputFile i = MkInputFileSizes [i.size]

-- | The file stat keys and sizes of the input files, sorted.
newtype KeyedInputFileSizes = MkKeyedInputFileSizes [(Maybe FileStatKey, FileSize)]
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

instance Semigroup KeyedInputFileSizes where
  MkKeyedInputFileSizes x <> MkKeyedInputFileSizes y =
    MkKeyedInputFileSizes (mergeSorted x y)

instance Monoid KeyedInputFileSizes where
  mempty = MkKeyedInputFileSizes []

instance FromInputFiles KeyedInputFileSizes where
  fromInputFile i = MkKeyedInputFileSizes [(i.fileStatKey, i.size)]

mergeSorted :: Ord a => [a] -> [a] -> [a]
mergeSorted xs [] = xs
mergeSorted [] ys = ys
mergeSorted (x : xs) (y : ys)
  | x <= y    = x : mergeSorted xs (y : ys)
  | otherwise = y : mergeSorted (x : xs) ys

-- | A stat key is the identity under which a task's resource usage is
-- recorded. The estimates are a model of the stat key and of an
-- 'InputSummary', a reduced view of the task's input files.
--
-- A field belongs in the stat key only if it can be computed from this task
-- without looking at other nodes of the task graph: no input sizes and no
-- input keys. Those belong in the summary.
--
-- A stat key should be a /reduced/ projection of a task key: fields that do
-- not affect resource usage (a numeric coupling, a file path) should be
-- dropped or coarsened, so that tasks differing only in those fields share
-- statistics.
--
-- A task's config ('Hyperion.Scheduler.Task.Task.TaskConfig') says /how/ to
-- compute, so it may legitimately affect estimates. Project the parts that do
-- (a version or variant tag) into the stat key when building it; never the
-- whole config, and never a filesystem path -- a path changes when nothing
-- about the computation changed, which would split recorded history for no
-- reason.
class (Typeable a, ToJSON a, FromJSON a) => IsStatKey a where
  -- | What the estimates need to know about the task's input files. The
  -- default '()' means that they ignore the inputs.
  type InputSummary a
  type InputSummary a = ()

  -- | Estimated memory in bytes: what a node must hold while this task runs,
  -- which is the resident set of the whole worker process and not only what
  -- the task itself allocates.
  --
  -- The distinction is easy to get wrong and costs an order of magnitude for a
  -- small task. Each concurrent task is its own worker, and
  -- 'Hyperion.Scheduler.RunTasks.NodeStatus.addTask' sums these figures, so
  -- each must carry the executable's own footprint -- tens of megabytes before
  -- the task allocates anything. Recorded statistics measure the same thing
  -- (the worker's peak resident set, its own or its children's), so a model
  -- that omits the baseline predicts too little.
  memoryEstimate :: a -> InputSummary a -> MemorySize

  -- | Estimated runtime in seconds, as a function of 'NumCPUs'.
  runtimeEstimate :: a -> InputSummary a -> NumCPUs -> NominalDiffTime
  runtimeEstimate k s = defaultRuntimeEstimate (memoryEstimate k s)

  -- | The tag written into stat files to identify this key's type. It is part
  -- of the on-disk format, so override it if you rename the type or move the
  -- module and want previously recorded statistics to keep matching.
  statKeyTypeName :: Text
  default statKeyTypeName :: Text
  statKeyTypeName = typeRepText @a

-- | The identity under which the size of an /output file/ is recorded.
--
-- Unlike 'IsStatKey' this takes no config: a file's size is a property of the
-- result (what was computed), and the config only describes how to compute it.
-- NB: 'FromJSON' is required by 'decodeFileStatKey' rather than by the class,
-- unlike 'IsStatKey'. A stat key is always a purpose-built reduced record, so
-- demanding round-trippability of it is cheap; a file stat key defaults to the
-- output key itself, and those are not generally parseable.
class (Typeable a, ToJSON a) => IsFileStatKey a where
  -- | What the size estimate needs to know about the input files of the task
  -- that produces this file.
  type ProducerInputSummary a
  type ProducerInputSummary a = ()

  -- | Estimated size of the file, in bytes.
  --
  -- Defaults to zero, i.e. unknown. That only under-counts node-local storage
  -- in 'canHandleTask' until a real size has been measured and recorded.
  fileSizeEstimate :: a -> ProducerInputSummary a -> FileSize
  fileSizeEstimate _ _ = 0

  -- | See 'statKeyTypeName'.
  fileStatKeyTypeName :: Text
  default fileStatKeyTypeName :: Text
  fileStatKeyTypeName = typeRepText @a

instance IsFileStatKey Void where
  fileSizeEstimate v _ = absurd v

-- | Tasks that are never scheduled by estimate -- placeholders, which are
-- replaced before the map is run, and no-ops, which perform no computation --
-- declare @type instance StatKeyOf MyKey = Void@ and return 'Nothing' from
-- @toStatKey@. There is no value to estimate from, hence 'absurd'.
--
-- Use that rather than an estimate of @0@ on a task that really does run: a
-- zero estimate is indistinguishable from a real one, and schedules the task
-- as though it were free.
instance IsStatKey Void where
  memoryEstimate v _ = absurd v

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
-- recorded and estimated. It is structural, i.e. a property of the key alone,
-- so that one definition serves both a produced file and a file already on
-- disk.
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

-- OutKey k = Void means no files.
instance ToFileStatKey Void

-- | A file is identified by its path: 'Eq' and 'Ord' compare nothing else.
-- A set of a task's files is then built without forcing their stat keys and
-- sizes, which can be expensive, and which
-- 'Hyperion.Scheduler.Task.EstimatedTaskMap.mkEstimatedTaskMap' may compute
-- from other tasks' file infos.
data TaskKeyFileInfo = MkTaskKeyFileInfo
  { path        :: VirtualFilePath
  , fileStatKey :: Maybe FileStatKey
  , fileSize    :: Estimate FileSize
    -- ^ Carries its provenance for the same reason a task's memory estimate
    -- does: 'Hyperion.Scheduler.Task.WrappedTask.decorateTaskWithStats'
    -- replaces it with a recorded size, and what the key itself declared is
    -- then the only way to tell whether 'fileSizeEstimate' is any good.
    --
    -- For an input file already on disk, it is the size on disk.
  }
  deriving (Generic, Show)

instance Eq TaskKeyFileInfo where
  x == y = x.path == y.path

instance Ord TaskKeyFileInfo where
  compare x y = compare x.path y.path

type ToTaskKeyFileInfo r a = (PathResolver r a, ToFileStatKey a)

-- | The file of the given key, of unknown (zero) size: the size of a file
-- depends on the inputs of the task producing it, or on the disk.
toTaskKeyFileInfo
  :: ToTaskKeyFileInfo r a
  => r -> a -> TaskKeyFileInfo
toTaskKeyFileInfo resolver key = MkTaskKeyFileInfo
  { path        = VirtualFilePath $ resolvePath resolver key
  , fileStatKey = toFileStatKey key
  , fileSize    = EstimatedByTask 0
  }

-- | What a summary sees of an input file: its file stat key and the size to
-- schedule with. Not the path, which can change when the computation did not,
-- and not the provenance of the size, which depends on whether statistics
-- exist.
inputFileSummary :: TaskKeyFileInfo -> InputFileSummary
inputFileSummary info = MkInputFileSummary
  { fileStatKey = info.fileStatKey
  , size        = schedulingEstimate info.fileSize
  }
