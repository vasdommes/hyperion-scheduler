{-# LANGUAGE ApplicativeDo           #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE DuplicateRecordFields   #-}
{-# LANGUAGE NoFieldSelectors        #-}
{-# LANGUAGE OverloadedRecordDot     #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE StaticPointers          #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Hyperion.Scheduler.Task.KeyTask where

import Bootstrap.Build                    (All, AllCF, FetchConfig (..), FetchT,
                                           Fetches, GetDependencies, HasForce,
                                           Keys, getDependencies, runFetchT)
import Bootstrap.Build.FList              (HasIndex (..), HasLength (..),
                                           Index (..), Length (..),
                                           Variant (..), setsFromLists,
                                           toVariants)
import Control.Monad.IO.Class             (MonadIO, liftIO)
import Data.Aeson                         (ToJSON (..), Value)
import Data.Binary                        (Binary (..))
import Data.Data                          (Proxy (..))
import Data.Kind                          (Constraint, Type)
import Data.Set                           (Set)
import Data.Set                           qualified as Set
import Data.Text                          qualified as Text
import Data.Time                          (NominalDiffTime)
import Data.Typeable                      (typeOf)
import GHC.Generics                       (Generic)
import Hyperion                           (Dict (..), Process, Static (..), cAp,
                                           cPure)
import Hyperion.Log                       qualified as Log
import Hyperion.OsPath                    (OsPath)
import Hyperion.Scheduler.IsTask          (IsTask (..), RunStage, Tag,
                                           memoryToCpuTimeApprox)
import Hyperion.Scheduler.PathResolver    (PathResolver (..),
                                           PathResolverForAll)
import Hyperion.Scheduler.Stats           (ToStatKey (..), mkStatKeyViaJSON)
import Hyperion.Scheduler.Task            (Task, mkTask)
import Hyperion.Scheduler.Task.KeyValue   (ValueType, readValue)
import Hyperion.Scheduler.Task.ListTask   (ListTask (..), listTaskLink)
import Hyperion.Scheduler.Task.Util       (contramapKey, emptyTaskChain,
                                           encodeBinaryFileAtomic, vAll)
import Hyperion.Scheduler.TaskKeyFileInfo (ToFileStatKey, ToTaskKeyFileInfo,
                                           toTaskKeyFileInfo)
import Hyperion.Scheduler.TaskLink        (TaskChain (TaskMerge, TaskNode),
                                           TaskLink (..))
import Hyperion.Scheduler.Types           (MemorySize, NumCPUs)
import Hyperion.Util.MonadPathExists      (MonadPathExists (..))
import Type.Reflection                    (Typeable)

type FetchesKey k = Fetches k (ValueType k)

type family ToKeyVals ks where
  ToKeyVals '[] = '[]
  ToKeyVals (k ': ks) = '(k, ValueType k) ': ToKeyVals ks

type family FetchesKeys ks m :: Constraint where
  FetchesKeys '[] m = ()
  FetchesKeys (k ': ks) m = (FetchesKey k m, FetchesKeys ks m)

data KeyTask r k = MkKeyTask
  { resolver :: r
  , config   :: KeyConfig k
  , key      :: k
  } deriving (Generic)

deriving instance (Binary k, Binary r, Binary (KeyConfig k)) => Binary (KeyTask r k)
deriving instance (ToJSON k, ToJSON r, ToJSON (KeyConfig k)) => ToJSON (KeyTask r k)
deriving instance (Eq k, Eq r, Eq (KeyConfig k)) => Eq (KeyTask r k)
deriving instance (Ord k, Ord r, Ord (KeyConfig k)) => Ord (KeyTask r k)

instance (Static (Binary r), Static (Binary k), Static (Binary (KeyConfig k)), Typeable r, Typeable k, Typeable (KeyConfig k)) => Static (Binary (KeyTask r k)) where
  closureDict = static (\Dict -> Dict) `cAp` closureDict @(Binary r, Binary (KeyConfig k), Binary k)

class Binary (ValueType k) => BinaryValue k
instance Binary (ValueType k) => BinaryValue k

fetchWithResolver
  :: forall ks r m .
     ( PathResolverForAll r ks
     , All BinaryValue ks
     , MonadIO m
     , HasLength ks
     )
  => Proxy ks
  -> r
  -> FetchConfig m (ToKeyVals ks)
fetchWithResolver _ resolver = go (getLength @ks)
  where
    go :: (PathResolverForAll r ks', All BinaryValue ks') => Length ks' -> FetchConfig m (ToKeyVals ks')
    go LZero      = FetchNil
    go (LSucc l') = liftIO . readValue resolver :&: go l'

keysAreKeysWitness :: forall ks . HasLength ks => Dict (Keys (ToKeyVals ks) ~ ks)
keysAreKeysWitness = go (getLength @ks)
  where
    go :: Length ks' -> Dict (Keys (ToKeyVals ks') ~ ks')
    go LZero = Dict
    go (LSucc l') = case go l' of
      Dict -> Dict

monoidListWitness :: forall ks . HasLength ks => Dict (AllCF Monoid [] ks)
monoidListWitness = go (getLength @ks)
  where
    go :: Length ks' -> Dict (AllCF Monoid [] ks')
    go LZero = Dict
    go (LSucc l') = case go l' of
      Dict -> Dict

semigroupListWitness :: forall ks . HasLength ks => Dict (AllCF Semigroup [] ks)
semigroupListWitness = go (getLength @ks)
  where
    go :: Length ks' -> Dict (AllCF Semigroup [] ks')
    go LZero = Dict
    go (LSucc l') = case go l' of
      Dict -> Dict

class ( All Ord (DepKeys k)
      , All Eq (DepKeys k)
      , All ToFileStatKey (DepKeys k)
      , All BinaryValue (DepKeys k)
      , HasLength (DepKeys k)
      , FetchesKeys (DepKeys k) (GetDependencies (DepKeyVals k))
      , FetchesKeys (DepKeys k) (FetchT (DepKeyVals k) Process)
      , Show k
      , ToJSON k
      , Binary k
      , Typeable k
      , ToFileStatKey k
      , Binary (KeyConfig k)
      , ToJSON (KeyConfig k)
      ) => BuildKey k where

  type DepKeys k :: [Type]
  type KeyConfig k :: Type
  type KeyConfig k = ()


  computeValue :: (Applicative f, HasForce f, FetchesKeys (DepKeys k) f) => KeyConfig k -> k -> f (ValueType k)
  computeValue = computeValueWithNumCpus 1

  computeValueWithNumCpus :: (Applicative f, HasForce f, FetchesKeys (DepKeys k) f) => NumCPUs -> KeyConfig k -> k -> f (ValueType k)

  computeValueM :: (Applicative f, HasForce f, FetchesKeys (DepKeys k) f) => KeyConfig k -> k -> f (Process (ValueType k))
  computeValueM = computeValueWithNumCpusM 1

  computeValueWithNumCpusM :: (Applicative f, HasForce f, FetchesKeys (DepKeys k) f) => NumCPUs -> KeyConfig k -> k -> f (Process (ValueType k))
  computeValueWithNumCpusM numCpus cfg = fmap pure . computeValueWithNumCpus numCpus cfg

  -- | Estimated memory in bytes
  keyTaskMemoryEstimate     :: k -> MemorySize
  keyTaskMemoryEstimate = const 0
  -- | Estimated runtime in seconds, as a function of NumCPUs
  keyTaskRuntimeEstimate    :: k -> NumCPUs -> NominalDiffTime
  keyTaskRuntimeEstimate t numCpus = memoryToCpuTimeApprox (keyTaskMemoryEstimate t) / fromIntegral numCpus
  -- | Maximum possible threads for the task
  -- TODO: get rid of RunStage?
  keyTaskMaxThreads :: RunStage -> k -> NumCPUs
  keyTaskMaxThreads _ _ = 1
  -- | Minimum possible threads for the keyTask
  keyTaskMinThreads :: RunStage -> k -> NumCPUs
  keyTaskMinThreads _ _ = 1
  keyTaskTag        :: k -> Maybe Tag
  keyTaskTag = Just . Text.pack . show . typeOf
  keyTaskDefaultPriority :: k -> Int
  keyTaskDefaultPriority = const 0

  saveValue :: Proxy k -> OsPath -> ValueType k -> Process ()
  default saveValue :: Binary (ValueType k) => Proxy k -> OsPath -> ValueType k -> Process ()
  saveValue _ path val = liftIO $ encodeBinaryFileAtomic path val

type DepKeyVals k = ToKeyVals (DepKeys k)

dependencies :: forall k . BuildKey k => KeyConfig k -> k -> Set (Variant (DepKeys k))
dependencies cfg key =
  case keysAreKeysWitness @(DepKeys k) of
    Dict -> case semigroupListWitness @(DepKeys k) of
      Dict -> case monoidListWitness @(DepKeys k) of
        Dict -> toVariants $ setsFromLists $ getDependencies @(DepKeyVals k) $ computeValue cfg key

computeAndWrite
  :: forall r k .
     Dict ( PathResolverForAll r (DepKeys k)
          , PathResolver r k
          , BuildKey k
          )
  -> Int
  -> KeyTask r k
  -> Process ()
computeAndWrite Dict numCpus task = do
  val <- runFetchT (computeValueWithNumCpus numCpus task.config task.key) (fetchWithResolver (Proxy @(DepKeys k)) task.resolver)
  let path = resolvePath task.resolver task.key
  Log.info "Saving value" (task.key, path)
  saveValue (Proxy @k) path val

class ToTaskKeyFileInfo r k => FileInfo r k
instance ToTaskKeyFileInfo r k => FileInfo r k

instance
  ( Static(PathResolverForAll r (DepKeys k))
  , All (FileInfo r) (DepKeys k)
  , PathResolver r k
  , BuildKey k
  , ToJSON (KeyTask r k)
  , Eq (KeyTask r k)
  , Ord (KeyTask r k)
  , All (FileInfo r) (DepKeys k)
  , Static (PathResolver r k)
  , Static (BuildKey k)
  , Static (Binary r)
  , Static (Binary k)
  , Static (Binary (KeyConfig k))
  , Typeable r
  , Typeable k
  , Typeable (KeyConfig k)
  , Typeable (PathResolverForAll r (DepKeys k))
  ) => IsTask (KeyTask r k) where
  taskMemoryEstimate t   = keyTaskMemoryEstimate t.key
  taskRuntimeEstimate t  = keyTaskRuntimeEstimate t.key
  -- TODO reorder arguments?
  taskMaxThreads stage t = keyTaskMaxThreads stage t.key
  taskMinThreads stage t = keyTaskMinThreads stage t.key
  taskInputs t           = Set.map toFileInfo $ dependencies t.config t.key where
    toFileInfo = vAll @(FileInfo r) (toTaskKeyFileInfo t.resolver)
  taskOutputs t          = Set.singleton $ toTaskKeyFileInfo t.resolver t.key
  taskDefaultPriority t  = keyTaskDefaultPriority t.key
  taskTag t              = keyTaskTag t.key
  taskClosure numCpus t  = Just $ static computeAndWrite
    `cAp` closureDict
    `cAp` cPure numCpus
    `cAp` cPure t

instance {-# OVERLAPPABLE #-} BuildKey k => ToStatKey k where
  toStatKey = mkStatKeyViaJSON

instance {-# OVERLAPPABLE #-} ToStatKey k => ToStatKey (KeyTask r k) where
  toStatKey t = toStatKey t.key

keyTaskLink
  :: forall r c k m. (MonadPathExists m, HasConfig c (KeyConfig k), BuildKey k, PathResolver r k)
  => r
  -> c
  -> TaskLink m k (Variant (DepKeys k)) (KeyTask r k)
keyTaskLink resolver cfg' = MkTaskLink
  { dependencies = dependencies @k cfg
  , checkCreated = doesPathExist . resolvePath resolver
  , toTask = MkKeyTask resolver cfg
  }
  where
    cfg = toConfig cfg'

class HasConfig a b where
  toConfig :: a -> b

instance HasConfig a () where
  toConfig = const ()

class HasTaskChain m r c k t where
  taskChain :: r -> c -> TaskChain m k t

instance {-# OVERLAPPING #-} Applicative m => HasTaskChain m r c (Variant '[]) Task where
  taskChain _ _ = emptyTaskChain

instance {-# OVERLAPPING #-} (HasTaskChain m r c k Task, HasTaskChain m r c (Variant ks) Task) => HasTaskChain m r c (Variant (k ': ks)) Task where
  taskChain resolver cfg = TaskMerge (taskChain resolver cfg) (taskChain resolver cfg)

instance {-# OVERLAPPABLE #-}
  ( Static (BuildKey k)
  , HasTaskChain m r c (Variant (DepKeys k)) Task
  , MonadPathExists m, HasConfig c (KeyConfig k)
  , Static (PathResolver r k)
  , Static (PathResolverForAll r (DepKeys k))
  , All (FileInfo r) (DepKeys k)
  , Static (Binary r)
  , Static (Binary k)
  , IsTask (KeyTask r k)
  , Typeable r
  , Typeable k
  , ToJSON r
  ) => HasTaskChain m r c k Task where
  taskChain resolver cfg = TaskNode (mkTask <$> keyTaskLink resolver cfg) (taskChain resolver cfg)

instance (Ord k, Typeable k, Binary k, ToJSON k, Applicative m, HasTaskChain m r c k Task) => HasTaskChain m r c [k] Task where
  taskChain resolver cfg = TaskNode (mkTask <$> listTaskLink) (taskChain resolver cfg)

instance (Ord k, Typeable k, Binary k, ToJSON k, Applicative m, HasTaskChain m r c k Task) => HasTaskChain m r c (Set k) Task where
  taskChain resolver cfg = TaskNode (mkTask <$> contramapKey Set.toList listTaskLink) (taskChain resolver cfg)

pairTaskLink
  :: (Ord k, Ord k', Applicative m)
  => TaskLink m (k, k') (Variant '[k, k']) (ListTask (k, k'))
pairTaskLink = MkTaskLink
  { dependencies = \(k, k') -> Set.fromList [VLeft k, VRight (VLeft k')]
  , checkCreated = const (pure False)
  , toTask = \key -> MkListTask [key]
  }

instance
  ( Ord k
  , Ord k'
  , Typeable k
  , Typeable k'
  , Binary k
  , Binary k'
  , ToJSON k
  , ToJSON k'
  , Applicative m
  , HasTaskChain m r c k Task
  , HasTaskChain m r c k' Task
  ) => HasTaskChain m r c (k, k') Task where
  taskChain resolver cfg = TaskNode (mkTask <$> pairTaskLink) (taskChain resolver cfg)

data Configs (cs :: [Type]) where
  CNil  :: Configs '[]
  CCons :: c -> Configs cs -> Configs (c ': cs)

deriving instance All Show cs => Show (Configs cs)
deriving instance All Eq cs => Eq (Configs cs)
deriving instance (All Eq cs, All Ord cs) => Ord (Configs cs)

instance (HasLength cs, All Binary cs) => Binary (Configs cs) where
  put xs = case (xs, getLength @cs) of
    (CNil, LZero)          -> pure ()
    (CCons x xs', LSucc _) -> put x >> put xs'
  get = case getLength @cs of
    LZero -> pure CNil
    LSucc (_ :: Length cs') -> do
      x <- get
      fmap (CCons x) (get @(Configs cs'))

instance All ToJSON cs => ToJSON (Configs cs) where
  toJSON = toJSON . go
    where
      go :: All ToJSON cs' => Configs cs' -> [Value]
      go CNil         = []
      go (CCons x xs) = toJSON x : go xs

instance {-# OVERLAPPING #-} HasConfig (Configs cs) () where
  toConfig = const ()

instance {-# OVERLAPPABLE #-} HasIndex c cs => HasConfig (Configs cs) c where
  toConfig = go (index @c @cs)
    where
      go :: Index a as -> Configs as -> a
      go Here (CCons cfg _)        = cfg
      go (There i') (CCons _ cfgs) = go i' cfgs
