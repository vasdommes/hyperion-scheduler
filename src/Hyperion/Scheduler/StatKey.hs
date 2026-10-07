{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE DerivingStrategies      #-}
{-# LANGUAGE DerivingVia             #-}
{-# LANGUAGE DuplicateRecordFields   #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE NoFieldSelectors        #-}
{-# LANGUAGE OverloadedRecordDot     #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Stat keys, input summaries and file infos: what a task's estimates are
-- computed from and its statistics recorded under. See
-- @docs/stat-key-inputs-design.md@ for the design.
module Hyperion.Scheduler.StatKey where

import Control.DeepSeq                 (NFData)
import Control.Monad                   (guard)
import Data.Aeson                      (FromJSON, FromJSONKey, ToJSON (..),
                                        ToJSONKey, (.:), (.=))
import Data.Aeson                      qualified as Aeson
import Data.Aeson.Types                qualified as Aeson
import Data.Monoid                     (Sum (..))
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
import Hyperion.Scheduler.Util         (qualifiedTypeRepText)

newtype StatKey = MkStatKey Aeson.Value
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON, NFData)
  deriving anyclass (ToJSONKey, FromJSONKey)

-- | What every task knows about one of its input files, whatever the key type.
data InputFile = MkInputFile
  { fileStatKey :: Maybe FileStatKey
  , size        :: FileSize
  }

-- | A summary of a task's input files, built one file at a time without the
-- dependency key types. The logic of a stock summary lives here, once.
class Monoid s => FromInputFiles s where
  fromInputFile :: InputFile -> s

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
  deriving (Semigroup, Monoid) via (Sum FileSize)

instance FromInputFiles TotalInputFileSize where
  fromInputFile i = MkTotalInputFileSize i.size

-- | The sizes of the input files, sorted (a multiset).
newtype InputFileSizes = MkInputFileSizes [FileSize]
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)
  deriving (Semigroup, Monoid) via (Sorted FileSize)

instance FromInputFiles InputFileSizes where
  fromInputFile i = MkInputFileSizes [i.size]

-- | The file stat keys and sizes of the input files, sorted.
newtype KeyedInputFileSizes =
  MkKeyedInputFileSizes [(Maybe FileStatKey, FileSize)]
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)
  deriving (Semigroup, Monoid) via (Sorted (Maybe FileStatKey, FileSize))

instance FromInputFiles KeyedInputFileSizes where
  fromInputFile i = MkKeyedInputFileSizes [(i.fileStatKey, i.size)]

-- | A sorted list, merged by '<>'.
newtype Sorted a = MkSorted [a]

instance Ord a => Semigroup (Sorted a) where
  MkSorted xs <> MkSorted ys = MkSorted (merge xs ys)
    where
      merge as [] = as
      merge [] bs = bs
      merge (a : as) (b : bs)
        | a <= b    = a : merge as (b : bs)
        | otherwise = b : merge (a : as) bs

instance Ord a => Monoid (Sorted a) where
  mempty = MkSorted []

-- | A summary of a task's input files, see 'InputSummary'.
class (Ord s, ToJSON s, FromJSON s) => IsSummary s where
  -- | How far apart two summaries are, 'Nothing' if they cannot be compared.
  -- Statistics recorded with one summary correct the estimates for another
  -- only when the two are close (see 'closeInputSummaries').
  --
  -- The default makes a summary categorical: comparable only to itself. By
  -- default, summaries at a distance of at most 1 are close
  -- ('nearSummaries'); the size summaries count doublings, so 1 is a factor
  -- of 2.
  summaryDistance :: s -> s -> Maybe Double
  summaryDistance x y = if x == y then Just 0 else Nothing

-- | At a distance between 0 and 1, by 'summaryDistance': for sizes, within a
-- factor of 2 of each other.
nearSummaries :: IsSummary s => s -> s -> Bool
nearSummaries x y = maybe False (\d -> 0 <= d && d <= 1) (summaryDistance x y)

-- | The distance of two sizes: the log2 of their ratio, i.e. how many doublings
-- apart they are. One byte is added to each, so that empty files are
-- comparable.
sizeDistance :: FileSize -> FileSize -> Double
sizeDistance x y = abs $ logBase 2 $ (fromIntegral x + 1) / (fromIntegral y + 1)

-- | Equal lists of keys (or of nothing), compared by their sizes.
sizesDistance :: Eq k => [(k, FileSize)] -> [(k, FileSize)] -> Maybe Double
sizesDistance xs ys
  | map fst xs == map fst ys =
      Just $ maximum $ 0 : zipWith sizeDistance (map snd xs) (map snd ys)
  | otherwise = Nothing

instance IsSummary ()

instance (IsSummary a, IsSummary b) => IsSummary (a, b) where
  summaryDistance (a, b) (a', b') =
    max <$> summaryDistance a a' <*> summaryDistance b b'

instance IsSummary MaxInputFileSize where
  summaryDistance (MkMaxInputFileSize x) (MkMaxInputFileSize y) =
    Just (sizeDistance x y)

instance IsSummary TotalInputFileSize where
  summaryDistance (MkTotalInputFileSize x) (MkTotalInputFileSize y) =
    Just (sizeDistance x y)

-- | Comparable when they have as many files.
instance IsSummary InputFileSizes where
  summaryDistance (MkInputFileSizes xs) (MkInputFileSizes ys) =
    sizesDistance (map ((),) xs) (map ((),) ys)

-- | Comparable when they have the same file stat keys.
instance IsSummary KeyedInputFileSizes where
  summaryDistance (MkKeyedInputFileSizes xs) (MkKeyedInputFileSizes ys) =
    sizesDistance xs ys

-- | A summary, serialized like a 'StatKey'.
newtype EncodedSummary = MkEncodedSummary Aeson.Value
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON, NFData)
  deriving anyclass (ToJSONKey, FromJSONKey)

encodeSummary :: ToJSON s => s -> EncodedSummary
encodeSummary = MkEncodedSummary . toJSON

-- | The summary of estimates that ignore the inputs.
unitSummary :: EncodedSummary
unitSummary = encodeSummary ()

-- | 'Nothing' if the value does not parse, e.g. after the summary type
-- changed.
decodeSummary :: FromJSON s => EncodedSummary -> Maybe s
decodeSummary (MkEncodedSummary value) = Aeson.parseMaybe Aeson.parseJSON value

-- | A stat key is the identity under which a task's resource usage is
-- recorded. The estimates are a model of the stat key and of an
-- 'InputSummary', a reduced view of the task's input files.
--
-- Make it a /reduced/ projection of the task key, computed from this task
-- alone: input sizes and input keys belong in the summary. Drop or coarsen the
-- fields that do not affect resource usage, so that tasks differing only in
-- those share statistics. Project only the estimate-relevant parts of the
-- config (a version or variant tag), never a filesystem path.
class (Typeable a, ToJSON a, FromJSON a, IsSummary (InputSummary a))
  => IsStatKey a where
  -- | What the estimates need to know about the task's input files. The
  -- default '()' means that they ignore the inputs.
  type InputSummary a
  type InputSummary a = ()

  -- | Estimated memory in bytes: the resident set of the whole worker process
  -- while this task runs, not only what the task allocates. Each concurrent
  -- task is its own worker, so include the executable's own footprint (tens
  -- of megabytes); statistics measure the same thing, and a model without it
  -- is reported as under-predicting.
  memoryEstimate :: a -> InputSummary a -> MemorySize

  -- | Estimated runtime in seconds, as a function of 'NumCPUs'.
  runtimeEstimate :: a -> InputSummary a -> NumCPUs -> NominalDiffTime
  runtimeEstimate k s = defaultRuntimeEstimate (memoryEstimate k s)

  -- | Whether statistics recorded with the second summary can correct the
  -- estimates for the first. Override it when the model's ratio to the
  -- measurements stays valid over a different range.
  closeInputSummaries :: a -> InputSummary a -> InputSummary a -> Bool
  closeInputSummaries _ = nearSummaries

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
class (Typeable a, ToJSON a, IsSummary (ProducerSummary a))
  => IsFileStatKey a where
  -- | What the size estimate needs to know about the input files of the task
  -- that produces this file.
  type ProducerSummary a
  type ProducerSummary a = ()

  -- | Estimated size of the file, in bytes.
  --
  -- Defaults to zero, i.e. unknown. That only under-counts node-local storage
  -- in 'canHandleTask' until a real size has been measured and recorded.
  fileSizeEstimate :: a -> ProducerSummary a -> FileSize
  fileSizeEstimate _ _ = 0

  -- | 'closeInputSummaries' for 'fileSizeEstimate'.
  closeProducerSummaries :: a -> ProducerSummary a -> ProducerSummary a -> Bool
  closeProducerSummaries _ = nearSummaries

  -- | See 'statKeyTypeName'.
  fileStatKeyTypeName :: Text
  default fileStatKeyTypeName :: Text
  fileStatKeyTypeName = qualifiedTypeRepText @a

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
-- recorded and estimated. A property of the key alone, so that one definition
-- serves a produced file and a file already on disk. Defaults to 'Void': no
-- file statistics, and a size of zero, which only under-counts node-local
-- storage in @canHandleTask@. Prefer a /reduced/ projection: an unreduced key
-- puts every file in a group of its own, which cannot predict a new file.
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

-- | A file of a task: its path and file stat key, known before any size is.
-- A file is identified by its path: 'Eq' and 'Ord' compare nothing else, so a
-- set of files is built without forcing their stat keys, which can be
-- expensive.
data TaskFile = MkTaskFile
  { path        :: VirtualFilePath
  , fileStatKey :: Maybe FileStatKey
  }
  deriving (Generic, Show)

instance Eq TaskFile where
  x == y = x.path == y.path

instance Ord TaskFile where
  compare x y = compare x.path y.path

-- | A 'TaskFile' with its size. Compared by path, like 'TaskFile', so a set of
-- infos is built without forcing their sizes, which
-- 'Hyperion.Scheduler.Task.EstimatedTaskMap.mkEstimatedTaskMap' may compute
-- from other tasks' infos.
data SizedTaskFile = MkSizedTaskFile
  { path        :: VirtualFilePath
  , fileStatKey :: Maybe FileStatKey
  , fileSize    :: Estimate FileSize
    -- ^ Carries its provenance for the same reason a task's memory estimate
    -- does: 'Hyperion.Scheduler.Task.EstimatedTask.applyStats'
    -- replaces it with a recorded size, and what the key itself declared is
    -- then the only way to tell whether 'fileSizeEstimate' is any good.
    --
    -- For an input file already on disk, it is the size on disk.
  }
  deriving (Generic, Show)

instance Eq SizedTaskFile where
  x == y = x.path == y.path

instance Ord SizedTaskFile where
  compare x y = compare x.path y.path

-- | The file with the given size.
withSize :: Estimate FileSize -> TaskFile -> SizedTaskFile
withSize size file = MkSizedTaskFile
  { path        = file.path
  , fileStatKey = file.fileStatKey
  , fileSize    = size
  }

-- | A key whose file a resolver can name. A class rather than a constraint
-- synonym, so that it can be applied partially, e.g. in
-- @All (ToTaskFile r) ks@.
class (PathResolver r a, ToFileStatKey a) => ToTaskFile r a
instance (PathResolver r a, ToFileStatKey a) => ToTaskFile r a

-- | The file of the given key.
toTaskFile :: ToTaskFile r a => r -> a -> TaskFile
toTaskFile resolver key = MkTaskFile
  { path        = VirtualFilePath $ resolvePath resolver key
  , fileStatKey = toFileStatKey key
  }

-- | What a summary sees of an input file: its file stat key and the size to
-- schedule with. Not the path, which can change when the computation did not,
-- and not the provenance of the size, which depends on whether statistics
-- exist.
toInputFile :: SizedTaskFile -> InputFile
toInputFile info = MkInputFile
  { fileStatKey = info.fileStatKey
  , size        = schedulingEstimate info.fileSize
  }
