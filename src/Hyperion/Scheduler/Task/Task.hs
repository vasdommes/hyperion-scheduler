{-# LANGUAGE ApplicativeDo           #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE DerivingVia             #-}
{-# LANGUAGE DuplicateRecordFields   #-}
{-# LANGUAGE NoFieldSelectors        #-}
{-# LANGUAGE OverloadedRecordDot     #-}
{-# LANGUAGE StaticPointers          #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeAbstractions        #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Hyperion.Scheduler.Task.Task where

import Bootstrap.Build                     (All, FetchConfig (..),
                                            Fetches (..), FetchesAll, HasKVs,
                                            Keys, getDependenciesOf,
                                            runFetchTAll)
import Bootstrap.Build.FList               (FList, HasLength (..),
                                            Length (..), Variant (..), setsFromLists,
                                            toVariants)
import Bootstrap.Build.FList               qualified as FList
import Control.Distributed.Process         (Process)
import Control.Monad                       (join)
import Control.Monad.IO.Class              (MonadIO, liftIO)
import Data.Aeson                          (ToJSON (..))
import Data.Binary                         (Binary (..))
import Data.Binary                         qualified as Binary
import Data.Data                           (Proxy (..))
import Data.Functor.Compose                (Compose (..))
import Data.Kind                           (Constraint, Type)
import Data.Set                            (Set)
import Data.Set                            qualified as Set
import Data.Text                           qualified as Text
import Data.Time                           (NominalDiffTime)
import Data.Typeable                       (typeOf)
import GHC.Generics                        (Generic)
import Hyperion                            (Dict (..), Static (..), cAp, cPure)
import Hyperion.OsPath                     (OsPath)
import Hyperion.OsString                   qualified as OsString
import Hyperion.Scheduler.PathResolver     (PathResolver (..),
                                            PathResolverForAll)
import Hyperion.Scheduler.StatKey          (ToFileStatKey, ToStatKey (..),
                                            ToTaskKeyFileInfo, mkStatKeyViaJSON,
                                            toTaskKeyFileInfo)
import Hyperion.Scheduler.Task.HasConfig   (HasConfig (..))
import Hyperion.Scheduler.Task.IsTask      (IsTask (..), RunStage, Tag,
                                            memoryToCpuTimeApprox)
import Hyperion.Scheduler.Task.TaskLink    (HasTaskChain (..),
                                            TaskChain (TaskNode), TaskLink (..))
import Hyperion.Scheduler.Task.Util        (encodeBinaryFileAtomic, vAll)
import Hyperion.Scheduler.Task.WrappedTask (wrapTask)
import Hyperion.Scheduler.Types            (MemorySize, NumCPUs)
import Hyperion.Util.MonadPathExists       (MonadPathExists (..))
import Type.Reflection                     (Typeable)

type FetchesPath k = Fetches k OsPath

type family FetchesPaths ks f :: Constraint where
  FetchesPaths '[] f = ()
  FetchesPaths (k ': ks) f = (FetchesPath k f, FetchesPaths ks f)

type family WithPaths (ks :: [Type]) :: [(Type, Type)] where
  WithPaths '[] = '[]
  WithPaths (k ': ks) = '(k, OsPath) ': WithPaths ks

type OutAndDepKeys k = OutKey k ': DepKeys k
type OutAndDepsWithPaths k = WithPaths (OutAndDepKeys k)

data Task r k = MkTask
  { resolver :: r
  , config   :: TaskConfig k
  , key      :: k
  } deriving (Generic)

deriving instance (Binary k, Binary r, Binary (TaskConfig k)) => Binary (Task r k)
deriving instance (ToJSON k, ToJSON r, ToJSON (TaskConfig k)) => ToJSON (Task r k)
deriving instance (Eq k, Eq r, Eq (TaskConfig k)) => Eq (Task r k)
deriving instance (Ord k, Ord r, Ord (TaskConfig k)) => Ord (Task r k)

instance (Static (Binary r), Static (Binary k), Static (Binary (TaskConfig k)), Typeable r, Typeable k, Typeable (TaskConfig k)) => Static (Binary (Task r k)) where
  closureDict = static (\Dict -> Dict) `cAp` closureDict @(Binary r, Binary (TaskConfig k), Binary k)

fetchWithResolver
  :: forall ks r m .
     ( PathResolverForAll r ks
     , MonadIO m
     , HasLength ks
     )
  => Proxy ks
  -> r
  -> FetchConfig m (WithPaths ks)
fetchWithResolver _ resolver = go (getLength @ks)
  where
    go :: (PathResolverForAll r ks') => Length ks' -> FetchConfig m (WithPaths ks')
    go LZero      = FetchNil
    go (LSucc l') = pure . resolvePath resolver :&: go l'

keysAreKeysWitness :: forall ks . HasLength ks => Dict (Keys (WithPaths ks) ~ ks)
keysAreKeysWitness = go (getLength @ks)
  where
    go :: Length ks' -> Dict (Keys (WithPaths ks') ~ ks')
    go LZero = Dict
    go (LSucc l') = case go l' of
      Dict -> Dict

withPathsHasKVsWitness
  :: forall ks . HasLength ks
  => Proxy ks
  -> Dict (HasKVs (WithPaths ks))
withPathsHasKVsWitness _ = go (getLength @ks)
  where
    go :: Length ks' -> Dict (HasKVs (WithPaths ks'))
    go LZero = Dict
    go (LSucc l') = case go l' of
      Dict -> Dict

fetchesAllWithPathsWitness
  :: forall ks f . (HasLength ks, FetchesAll (WithPaths ks) f)
  => Proxy ks
  -> Proxy f
  -> Dict (FetchesPaths ks f)
fetchesAllWithPathsWitness _ _ = go (getLength @ks)
  where
    go
      :: FetchesAll (WithPaths ks') f
      => Length ks'
      -> Dict (FetchesPaths ks' f)
    go LZero = Dict
    go (LSucc l') = case go l' of
      Dict -> Dict

type family DepKeys k :: [Type]

class ( All Eq (DepKeys k)
      , All Ord (DepKeys k)
      , All ToFileStatKey (DepKeys k)
      , HasLength (DepKeys k)
      , Eq (OutKey k)
      , Ord (OutKey k)
      , ToFileStatKey (OutKey k)
      , Typeable (OutKey k)
      , Show k
      , ToJSON k
      , Binary k
      , Typeable k
      , Binary (TaskConfig k)
      , ToJSON (TaskConfig k)
      ) => TaskKey k where

  type OutKey k :: Type
  type OutKey k = k

  type TaskConfig k :: Type
  type TaskConfig k = ()

  -- This will be called with two f's:
  -- 1. GetDependencies (OutAndDepsWithPaths k) to get the dependencies of the key.
  -- 2. FetchT (OutAndDepsWithPaths k) Process to actually compute the value and write it to disk
  computeAndSaveValue
    :: ( Applicative f
       , FetchesPaths (OutKey k ': DepKeys k) f
       )
    => NumCPUs -> TaskConfig k -> k -> f (Process ())
  default computeAndSaveValue :: (Applicative f, ComputeValue k, ValueSerializable k, FetchesPaths (OutKey k ': DepKeys k) f, OutKey k ~ k) => NumCPUs -> TaskConfig k -> k -> f (Process ())
  computeAndSaveValue numCpus cfg key = do
    path <- getPath key
    getVal <- unWrappedProcess (computeValue numCpus cfg key)
    pure $ do
      val <- getVal
      saveValue key path val

  -- | Estimated memory in bytes
  memoryEstimate     :: k -> MemorySize
  memoryEstimate = const 0
  -- | Estimated runtime in seconds, as a function of NumCPUs
  runtimeEstimate    :: k -> NumCPUs -> NominalDiffTime
  runtimeEstimate t numCpus = memoryToCpuTimeApprox (memoryEstimate t) / fromIntegral numCpus
  -- | Maximum possible threads for the task
  -- TODO: get rid of RunStage?
  maxThreads :: RunStage -> k -> NumCPUs
  maxThreads _ _ = 1
  -- | Minimum possible threads for the keyTask
  minThreads :: RunStage -> k -> NumCPUs
  minThreads _ _ = 1
  tag        :: k -> Maybe Tag
  tag = Just . Text.pack . show . typeOf
  priority :: k -> Int
  priority = const 0

class ComputeValue k where
  {-# MINIMAL computeValue | computeValueM #-}

  computeValue :: (Applicative f, FetchesPaths (DepKeys k) f) => NumCPUs -> TaskConfig k -> k -> WrappedProcess f (ValueType k)
  computeValue numCpus cfg key = joinWrapped $ computeValueM numCpus cfg key

  computeValueM :: (Applicative f, FetchesPaths (DepKeys k) f) => NumCPUs -> TaskConfig k -> k -> WrappedProcess f (Process (ValueType k))
  computeValueM numCpus cfg key = pure <$> computeValue numCpus cfg key

type family ValueType k :: Type

class ValueSerializable k where
  readValue :: k -> OsPath -> Process (ValueType k)
  default readValue :: (Binary (ValueType k)) => k -> OsPath -> Process (ValueType k)
  readValue _ path = liftIO $ Binary.decodeFile (OsString.toString path)

  saveValue :: k -> OsPath -> ValueType k -> Process ()
  default saveValue :: (Binary (ValueType k)) => k -> OsPath -> ValueType k -> Process ()
  saveValue _ path value = liftIO $ encodeBinaryFileAtomic path value

type FetchesKey k = Fetches k (ValueType k)

-- type family ToKeyVals ks where
--   ToKeyVals '[] = '[]
--   ToKeyVals (k ': ks) = '(k, ValueType k) ': ToKeyVals ks

type family FetchesKeys ks m :: Constraint where
  FetchesKeys '[] m = ()
  FetchesKeys (k ': ks) m = (FetchesKey k m, FetchesKeys ks m)

newtype WrappedProcess f a = MkWrappedProcess (Compose f Process a)
  deriving newtype (Applicative, Functor)

instance (Functor f, FetchesPath k f, ValueSerializable k, v ~ ValueType k) => Fetches k v (WrappedProcess f) where
  fetch key = MkWrappedProcess . Compose . fmap (readValue key) $ getPath key

unWrappedProcess :: WrappedProcess f a -> f (Process a)
unWrappedProcess (MkWrappedProcess (Compose x)) = x

joinWrapped :: Functor f => WrappedProcess f (Process a) -> WrappedProcess f a
joinWrapped (MkWrappedProcess (Compose f)) = MkWrappedProcess (Compose (fmap join f))

getPath :: Fetches k OsPath f => k -> f OsPath
getPath = fetch

outAndDependencies :: forall k . TaskKey k => TaskConfig k -> k -> FList Set (OutAndDepKeys k)
outAndDependencies cfg key =
  case keysAreKeysWitness @(OutAndDepKeys k) of
    Dict -> case withPathsHasKVsWitness (Proxy @(OutAndDepKeys k)) of
      Dict -> setsFromLists $ getDependenciesOf (Proxy @(OutAndDepsWithPaths k)) fetchAction
  where
    fetchAction
      :: forall f . (Applicative f, FetchesAll (OutAndDepsWithPaths k) f)
      => f (Process ())
    fetchAction =
      case fetchesAllWithPathsWitness (Proxy @(OutAndDepKeys k)) (Proxy @f) of
        Dict -> computeAndSaveValue 1 cfg key

dependencies :: forall k . TaskKey k => TaskConfig k -> k -> Set (Variant (DepKeys k))
dependencies cfg key = toVariants . FList.tail $ outAndDependencies cfg key

outKeys :: forall k . TaskKey k => TaskConfig k -> k -> Set (OutKey k)
outKeys cfg key = FList.head $ outAndDependencies cfg key

computeAndWrite
  :: forall r k .
     Dict ( PathResolverForAll r (DepKeys k)
          , PathResolver r (OutKey k)
          , TaskKey k
          )
  -> Int
  -> Task r k
  -> Process ()
computeAndWrite Dict numCpus task =
  join $
  runFetchTAll fetchAction $
  fetchWithResolver (Proxy @(OutKey k ': DepKeys k)) task.resolver
  where
    fetchAction
      :: forall f . (Applicative f, FetchesAll (OutAndDepsWithPaths k) f)
      => f (Process ())
    fetchAction =
      case fetchesAllWithPathsWitness (Proxy @(OutAndDepKeys k)) (Proxy @f) of
        Dict -> computeAndSaveValue numCpus task.config task.key

class ToTaskKeyFileInfo r k => FileInfo r k
instance ToTaskKeyFileInfo r k => FileInfo r k

instance
  ( Static(PathResolverForAll r (DepKeys k))
  , All (FileInfo r) (DepKeys k)
  , TaskKey k
  , ToJSON (Task r k)
  , Eq (Task r k)
  , Ord (Task r k)
  , All (FileInfo r) (DepKeys k)
  , Static (PathResolver r (OutKey k))
  , Static (TaskKey k)
  , Static (Binary r)
  , Static (Binary k)
  , Static (Binary (TaskConfig k))
  , Typeable r
  , Typeable k
  , Typeable (TaskConfig k)
  , Typeable (PathResolverForAll r (DepKeys k))
  ) => IsTask (Task r k) where
  taskMemoryEstimate t   = memoryEstimate t.key
  taskRuntimeEstimate t  = runtimeEstimate t.key
  -- TODO reorder arguments?
  taskMaxThreads stage t = maxThreads stage t.key
  taskMinThreads stage t = minThreads stage t.key
  taskInputs t           = Set.map toFileInfo $ dependencies t.config t.key where
    toFileInfo = vAll @(FileInfo r) (toTaskKeyFileInfo t.resolver)
  taskOutputs t          = Set.map (toTaskKeyFileInfo t.resolver) $ outKeys t.config t.key
  taskDefaultPriority t  = priority t.key
  taskTag t              = tag t.key
  taskClosure numCpus t  = Just $ static computeAndWrite
    `cAp` closureDict
    `cAp` cPure numCpus
    `cAp` cPure t

instance {-# OVERLAPPABLE #-} TaskKey k => ToStatKey k where
  toStatKey = mkStatKeyViaJSON

instance {-# OVERLAPPABLE #-} ToStatKey k => ToStatKey (Task r k) where
  toStatKey t = toStatKey t.key

taskLink
  :: forall r c k m. (MonadPathExists m, HasConfig c (TaskConfig k), TaskKey k, PathResolver r k)
  => r
  -> c
  -> TaskLink m k (Variant (DepKeys k)) (Task r k)
taskLink resolver cfg' = MkTaskLink
  { dependencies = dependencies @k cfg
  , checkCreated = doesPathExist . resolvePath resolver
  , toTask = MkTask resolver cfg
  }
  where
    cfg = toConfig cfg'

instance {-# OVERLAPPABLE #-}
  ( Static (TaskKey k)
  , HasTaskChain m r c (Variant (DepKeys k))
  , MonadPathExists m
  , HasConfig c (TaskConfig k)
  , Static (PathResolver r k)
  , Static (PathResolverForAll r (DepKeys k))
  , All (FileInfo r) (DepKeys k)
  , Static (Binary r)
  , Static (Binary k)
  , IsTask (Task r k)
  ) => HasTaskChain m r c k where
  taskChain resolver cfg = TaskNode (wrapTask <$> taskLink resolver cfg) (taskChain resolver cfg)
