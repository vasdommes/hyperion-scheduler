{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ApplicativeDo           #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE DerivingVia             #-}
{-# LANGUAGE DuplicateRecordFields   #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE NoFieldSelectors        #-}
{-# LANGUAGE OverloadedRecordDot     #-}
{-# LANGUAGE StaticPointers          #-}
{-# LANGUAGE TypeAbstractions        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Hyperion.Scheduler.Task.Task where

import Bootstrap.Build                     (All, FList, FetchConfig (..),
                                            Fetches (..), FetchesAll,
                                            HasForce (..), Keys, KnownKeyVals,
                                            KnownLength (..), Length (..),
                                            MultiList, Variant (..),
                                            getDependencies, headF,
                                            runFetchTAll,
                                            runMemoFetchTWithDependencies,
                                            setsFromLists, tailF, toVariants,
                                            vAll)
import Control.Distributed.Process         (Process)
import Control.Monad                       (join)
import Control.Monad.IO.Class              (MonadIO, liftIO)
import Data.Aeson                          (ToJSON (..))
import Data.Binary                         (Binary (..))
import Data.Binary                         qualified as Binary
import Data.Data                           (Proxy (..))
import Data.Foldable.Extra                 (allM, traverse_)
import Data.Functor                        (($>), (<&>))
import Data.Kind                           (Constraint, Type)
import Data.Map.Strict                     qualified as Map
import Data.Maybe                          (isJust)
import Data.Set                            (Set)
import Data.Set                            qualified as Set
import Data.Text                           (Text)
import Data.Typeable                       (cast, typeOf)
import Data.Void                           (Void)
import GHC.Generics                        (Generic)
import Hyperion                            (Dict (..), Static (..), cAp, cPure)
import Hyperion.OsPath                     (OsPath)
import Hyperion.OsString                   qualified as OsString
import Hyperion.Scheduler.PathResolver     (MapResolver (..), PathResolver (..),
                                            PathResolverForAll,
                                            mapResolverForAllDict)
import Hyperion.Scheduler.StatKey          (FromInputFiles (..), InputFile,
                                            IsFileStatKey (..), IsStatKey (..),
                                            TaskFile (..), ToFileStatKey (..),
                                            ToTaskFile, decodeSummary,
                                            encodeStatKey, encodeSummary,
                                            toInputFile, toTaskFile, withSize)
import Hyperion.Scheduler.Task.HasConfig   (HasConfig (..))
import Hyperion.Scheduler.Task.IsTask      (IsTask (..), Model (..),
                                            ResourceEstimates (..), RunStage,
                                            Tag, TaskEstimation (..),
                                            TaskShape (..), defaultTaskTag,
                                            defaultTaskTagForType,
                                            estimatesFromModel)
import Hyperion.Scheduler.Task.TaskLink    (HasTaskChain (..),
                                            TaskChain (TaskNode), TaskLink (..))
import Hyperion.Scheduler.Task.Util        (encodeBinaryFileAtomic)
import Hyperion.Scheduler.Task.WrappedTask (wrapTask)
import Hyperion.Scheduler.TaskFiles        (MonadTaskFiles, doesTaskFileExist)
import Hyperion.Scheduler.Types            (Estimate (..), NumCPUs)
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
     , KnownLength ks
     )
  => Proxy ks
  -> r
  -> FetchConfig m (WithPaths ks)
fetchWithResolver _ resolver = go (knownLength @ks)
  where
    go :: (PathResolverForAll r ks') => Length ks' -> FetchConfig m (WithPaths ks')
    go LZero      = FetchNil
    go (LSucc l') = pure . resolvePath resolver :&: go l'

keysAreKeysDict :: forall ks . KnownLength ks => Dict (Keys (WithPaths ks) ~ ks)
keysAreKeysDict = go (knownLength @ks)
  where
    go :: Length ks' -> Dict (Keys (WithPaths ks') ~ ks')
    go LZero = Dict
    go (LSucc l') = case go l' of
      Dict -> Dict

withPathsKnownKeyValsDict
  :: forall ks . KnownLength ks
  => Proxy ks
  -> Dict (KnownKeyVals (WithPaths ks))
withPathsKnownKeyValsDict _ = go (knownLength @ks)
  where
    go :: Length ks' -> Dict (KnownKeyVals (WithPaths ks'))
    go LZero = Dict
    go (LSucc l') = case go l' of
      Dict -> Dict

fetchesAllWithPathsDict
  :: forall ks f . (KnownLength ks, FetchesAll (WithPaths ks) f)
  => Proxy ks
  -> Proxy f
  -> Dict (FetchesPaths ks f)
fetchesAllWithPathsDict _ _ = go (knownLength @ks)
  where
    go
      :: FetchesAll (WithPaths ks') f
      => Length ks'
      -> Dict (FetchesPaths ks' f)
    go LZero = Dict
    go (LSucc l') = case go l' of
      Dict -> Dict

type family DepKeys k :: [Type]

-- | How a task computes and saves its value. This is a GADT so that each
-- constructor carries exactly the evidence its implementation needs: matching
-- on 'ComputeValueTask' brings 'ComputeValue' etc. into scope, so placeholder
-- tasks need no 'ComputeValue' instance, and vice versa. Every kind is
-- executable-or-rejected by construction -- there is no "forgot to implement
-- computeAndSaveValue" state (cf. the old class-method design, where a dummy
-- instance that forgot to fetch its output path silently produced an empty
-- 'outKeys' and was vacuously considered created).
data TaskKind k where
  -- | Compute the value with 'ComputeValue' and save it with
  -- 'ValueSerializable'. This is 'taskKind''s default.
  ComputeValueTask
    :: ( ComputeValue k
       , ValueSerializableM Process k
       , All (ValueSerializableM Process) (DepKeys k)
       , OutKey k ~ k
       )
    => TaskKind k
  -- | A stand-in for another task producing the same output file (@OutKey k ~
  -- k@ by construction, so the output is definitionally the key's own path,
  -- with no dependencies). It cannot be executed: it must be replaced (via
  -- 'Hyperion.Scheduler.Task.TaskMap.replaceTasks') before the map reaches
  -- 'Hyperion.Scheduler.RunTasks.runTasks', whose validation rejects
  -- unreplaced placeholders.
  PlaceholderTask
    :: (OutKey k ~ k, DepKeys k ~ '[])
    => TaskKind k
  -- | A fully custom computation. NB: it MUST 'getPath' every input it reads
  -- and every output it writes -- the task graph (outputs, dependency edges)
  -- is derived from exactly these calls, so a missed 'getPath' silently
  -- corrupts the graph.
  CustomTask
    :: (forall f . (Applicative f, FetchesPaths (OutKey k ': DepKeys k) f)
        => NumCPUs -> TaskConfig k -> k -> f (Process ()))
    -> TaskKind k
  -- | A task performing no computation: a pure grouping node whose only role
  -- is to depend on other tasks (@OutKey k ~ Void@: no output files). The
  -- given action must 'getPath' each dependency -- that is what declares the
  -- dependency edges. No closure is created for it and nothing is executed
  -- remotely, and it occupies zero worker threads (cf. 'remoteRunTask', which
  -- completes closure-less tasks instantly).
  NoOpTask
    :: OutKey k ~ Void
    => (forall f . (Applicative f, FetchesPaths (DepKeys k) f) => k -> f ())
    -> TaskKind k

-- | A task's dependencies with their files' stat keys and sizes.
type DepInputs k = [(Variant (DepKeys k), InputFile)]

class ( All Eq (DepKeys k)
      , All Ord (DepKeys k)
      , All ToFileStatKey (DepKeys k)
      , KnownLength (DepKeys k)
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
      , IsStatKey (StatKeyOf k)
      ) => TaskKey k where

  type OutKey k :: Type
  type OutKey k = k

  -- | TaskKey defines WHAT to compute, TaskConfig - HOW to compute it:
  -- e.g. which executable to call, which dependencies to fetch, how to parallelize etc.
  type TaskConfig k :: Type
  type TaskConfig k = ()

  -- | How this task computes and saves its value -- see 'TaskKind'.
  -- The actual computation ('computeAndSaveValue', a plain function dispatching on this)
  -- is run with two f's:
  -- 1. GetDependencies (OutAndDepsWithPaths k) to get the dependencies of the key.
  -- 2. FetchT (OutAndDepsWithPaths k) Process to actually compute the value and write it to disk
  taskKind :: TaskKind k
  default taskKind
    :: ( ComputeValue k
       , ValueSerializableM Process k
       , All (ValueSerializableM Process) (DepKeys k)
       , OutKey k ~ k
       )
    => TaskKind k
  taskKind = ComputeValueTask

  -- | StatKeyOf k is used for two things:
  -- 1. as a key for TaskStats (resource usage stats);
  -- 2. with the 'InputSummary', as the input to 'memoryEstimate' and
  --    'runtimeEstimate' (see 'IsStatKey').
  --
  -- Defaults to 'Void', i.e. no statistics: the task is neither recorded nor
  -- looked up, and both its estimates are zero. That is correct for tasks that
  -- compute nothing (NoOpTask), and tolerable for small ones:
  -- a task with runtimeEstimate=0 gets 'minThreads' and lower priority.
  -- 'Hyperion.Scheduler.Task.TaskMap.taskInstrumentationGaps' reports tasks
  -- that compute but declare no stat key, so you'll see them in the logs.
  --
  -- A stat key should be a /reduced/ projection of the key: drop or coarsen
  -- the fields that do not affect resource usage.
  type StatKeyOf k :: Type
  type StatKeyOf k = Void

  -- | Project this key (and the estimate-relevant parts of its config) onto
  -- its stat key.
  toStatKey :: TaskConfig k -> k -> Maybe (StatKeyOf k)
  toStatKey _ _ = Nothing

  -- | Summarize the task's direct dependencies for its estimates. A pure
  -- function of the dependency keys and the sizes of their files, so that it
  -- can be computed from estimated sizes as well as from measured ones.
  --
  -- The default builds a stock summary one file at a time (see
  -- 'FromInputFiles'). Override it for a summary that needs the typed
  -- dependency keys.
  toInputSummary
    :: TaskConfig k -> k -> DepInputs k -> InputSummary (StatKeyOf k)
  default toInputSummary
    :: FromInputFiles (InputSummary (StatKeyOf k))
    => TaskConfig k -> k -> DepInputs k -> InputSummary (StatKeyOf k)
  toInputSummary _ _ = summarizeInputFiles

  -- | 'toInputSummary' for the size estimates of the output files.
  toProducerSummary
    :: TaskConfig k
    -> k
    -> DepInputs k
    -> ProducerSummary (FileStatKeyOf (OutKey k))
  default toProducerSummary
    :: FromInputFiles (ProducerSummary (FileStatKeyOf (OutKey k)))
    => TaskConfig k
    -> k
    -> DepInputs k
    -> ProducerSummary (FileStatKeyOf (OutKey k))
  toProducerSummary _ _ = summarizeInputFiles

  -- | Maximum possible threads for the task.
  -- TODO: get rid of RunStage?
  maxThreads :: RunStage -> TaskConfig k -> k -> NumCPUs
  maxThreads = minThreads
  -- | Minimum possible threads for the keyTask
  -- NoOpTask's perform no computation, so they occupy no worker threads
  -- (e.g. ListTaskKey).
  minThreads :: RunStage -> TaskConfig k -> k -> NumCPUs
  minThreads _ _ _ = case taskKind @k of
    NoOpTask _ -> 0
    _          -> 1

  tag :: k -> Maybe Tag
  tag = defaultTaskTag
  priority :: k -> Int
  priority = const 0

  checkCreated
    :: (PathResolver r (OutKey k), MonadTaskFiles m)
    => TaskConfig k -> r -> k -> m Bool
  -- NB: a 'NoOpTask' has no output files, so the 'allM' check would be
  -- vacuously True and the task (with its dependency edges!) would always be
  -- pruned from the graph -- hence the explicit False.
  checkCreated cfg resolver key = case taskKind @k of
    NoOpTask _ -> pure False
    _ ->
      allM (doesTaskFileExist . resolvePath resolver) $ outKeys cfg key


-- | The (key, value) pairs of the given keys, where each value is the key's
-- 'ValueType'.
type family ValueKeyVals (ks :: [Type]) :: [(Type, Type)] where
  ValueKeyVals '[] = '[]
  ValueKeyVals (k ': ks) = '(k, ValueType k) ': ValueKeyVals ks

-- | What a 'ComputeValue' task fetches: its dependencies' values.
type DepKeyVals k = ValueKeyVals (DepKeys k)

-- | Compute a task's value from the values of its dependencies, fetched with
-- 'fetch'. The fetches define the task's dependencies. They must not depend
-- on 'NumCPUs': the task graph is built with @NumCPUs = 1@.
--
-- The computation is polymorphic in @f@, so that the scheduler can run it
-- with 'Bootstrap.Build.GetDependencies' to find the dependencies, and with
-- 'Bootstrap.Build.runMemoFetchTWithDependencies' to compute the value. The
-- latter reads each dependency once, even if it is fetched many times. Values
-- captured by the result's 'Process' action stay in memory until it runs.
--
-- Define 'computeValue' for a pure computation, or 'computeValueM' to compute
-- the value with effects in a 'Process' action run after the fetches.
class ComputeValue k where
  {-# MINIMAL computeValue | computeValueM #-}

  computeValue
    :: (Applicative f, HasForce f, FetchesAll (DepKeyVals k) f)
    => NumCPUs -> TaskConfig k -> k -> f (ValueType k)
  -- A computation with effects has no pure form. The scheduler calls only
  -- 'computeValueM'.
  computeValue = error "ComputeValue: computeValue is undefined for an instance that defines computeValueM"

  computeValueM
    :: (Applicative f, HasForce f, FetchesAll (DepKeyVals k) f)
    => NumCPUs -> TaskConfig k -> k -> f (Process (ValueType k))
  computeValueM numCpus cfg key = pure <$> computeValue numCpus cfg key

type family ValueType k :: Type

class ValueSerializable k where
  readValue :: k -> OsPath -> IO (ValueType k)
  default readValue :: Binary (ValueType k) => k -> OsPath -> IO (ValueType k)
  readValue _ path = Binary.decodeFile (OsString.toString path)

  saveValue :: k -> OsPath -> ValueType k -> IO ()
  default saveValue :: Binary (ValueType k) => k -> OsPath -> ValueType k -> IO ()
  saveValue _ = encodeBinaryFileAtomic

class ValueSerializableM m k where
  readValueM :: k -> OsPath -> m (ValueType k)
  saveValueM :: k -> OsPath -> ValueType k -> m ()

instance (MonadIO m, ValueSerializable k) => ValueSerializableM m k where
  readValueM key = liftIO . readValue key
  saveValueM key path = liftIO . saveValue key path

type FetchesKey k = Fetches k (ValueType k)

type family FetchesKeys ks m :: Constraint where
  FetchesKeys '[] m = ()
  FetchesKeys (k ': ks) m = (FetchesKey k m, FetchesKeys ks m)

getPath :: Fetches k OsPath f => k -> f OsPath
getPath = fetch

getPathVariant :: FetchesPaths ks f => Variant ks -> f OsPath
getPathVariant (VLeft x)  = getPath x
getPathVariant (VRight y) = getPathVariant y

-- | Compute the task's value and save it to disk, according to its
-- 'taskKind'. Not a class method: dispatching on the 'TaskKind' GADT here
-- means each kind's constraints come from its constructor, and every kind has
-- a consistent planning/execution behaviour by construction.
computeAndSaveValue
  :: forall k f .
     ( TaskKey k
     , Applicative f
     , FetchesPaths (OutKey k ': DepKeys k) f
     )
  => NumCPUs -> TaskConfig k -> k -> f (Process ())
computeAndSaveValue numCpus cfg key = case taskKind @k of
  ComputeValueTask -> do
    path <- getPath key
    depPaths <- traverse getPathVariant deps
    pure $ case mapResolverForAllDict @(DepKeys k) of
      Dict -> do
        let resolver = MkMapResolver (Map.fromList (zip deps depPaths))
        getVal <- runComputeValueM @k depList
          (computeValueM numCpus cfg key) resolver
        val <- getVal
        saveValueM key path val
    where
      -- Computed once, for both the resolver and the run. Same numCpus as the
      -- run, so the resolver has every key it fetches.
      depList = computeValueDependencyList numCpus cfg key
      deps = Set.toList (toVariants (setsFromLists depList))
  -- The 'getPath' call declares the placeholder's output (OutKey k ~ k),
  -- so its graph node has the same output path as the real task it stands in for.
  -- It should never execute: validateTaskMap rejects unreplaced placeholders.
  PlaceholderTask -> do
    _ <- getPath key
    pure $ do
      error $ "computeAndSaveValue: unreplaced placeholder task " <> show (typeOf key)
  CustomTask go -> go numCpus cfg key
  NoOpTask go -> go key $> pure ()

-- | Evidence about 'ValueKeyVals' that GHC cannot derive for an abstract
-- key list.
valueKeyValsDict
  :: forall ks . KnownLength ks
  => Dict (KnownKeyVals (ValueKeyVals ks), Keys (ValueKeyVals ks) ~ ks)
valueKeyValsDict = go (knownLength @ks)
  where
    go :: Length ks' -> Dict (KnownKeyVals (ValueKeyVals ks'), Keys (ValueKeyVals ks') ~ ks')
    go LZero      = Dict
    go (LSucc l') = case go l' of
      Dict -> Dict

-- | Fetch each key's value from the file that the resolver gives for it.
valueFetchConfig
  :: forall ks r m . (MonadIO m, All (ValueSerializableM m) ks, PathResolverForAll r ks)
  => Length ks -> r -> FetchConfig m (ValueKeyVals ks)
valueFetchConfig LZero _          = FetchNil
valueFetchConfig (LSucc l) resolver =
  (\key -> readValueM key (resolvePath resolver key)) :&: valueFetchConfig l resolver

-- | The dependencies of a 'ComputeValue' task: the keys that 'computeValueM'
-- fetches.
computeValueDependencies
  :: forall k . (TaskKey k, ComputeValue k)
  => NumCPUs -> TaskConfig k -> k -> Set (Variant (DepKeys k))
computeValueDependencies numCpus cfg key =
  toVariants . setsFromLists $ computeValueDependencyList numCpus cfg key

-- | Every fetch of 'computeValueM', with repeats.
computeValueDependencyList
  :: forall k . (TaskKey k, ComputeValue k)
  => NumCPUs -> TaskConfig k -> k -> MultiList (DepKeys k)
computeValueDependencyList numCpus cfg key =
  case valueKeyValsDict @(DepKeys k) of
    Dict ->
      getDependencies (Proxy @(DepKeyVals k)) (computeValueM numCpus cfg key)

-- | Run a 'ComputeValue' computation, reading the dependencies' values from
-- the files that the resolver gives. Each value is read once (see
-- 'runMemoFetchTWithDependencies'). The list must be the computation's
-- 'computeValueDependencyList'.
runComputeValueM
  :: forall k r a .
     ( TaskKey k
     , All (ValueSerializableM Process) (DepKeys k)
     , PathResolverForAll r (DepKeys k)
     )
  => MultiList (DepKeys k)
  -> (forall f . (Applicative f, HasForce f, FetchesAll (DepKeyVals k) f) => f a)
  -> r
  -> Process a
runComputeValueM deps action resolver = case valueKeyValsDict @(DepKeys k) of
  Dict -> runMemoFetchTWithDependencies @(DepKeyVals k) deps action $
    valueFetchConfig (knownLength @(DepKeys k)) resolver

outAndDependencies :: forall k . TaskKey k => TaskConfig k -> k -> FList Set (OutAndDepKeys k)
outAndDependencies cfg key =
  case keysAreKeysDict @(OutAndDepKeys k) of
    Dict -> case withPathsKnownKeyValsDict (Proxy @(OutAndDepKeys k)) of
      Dict -> setsFromLists $ getDependencies (Proxy @(OutAndDepsWithPaths k)) fetchAction
  where
    fetchAction
      :: forall f . (Applicative f, FetchesAll (OutAndDepsWithPaths k) f)
      => f (Process ())
    fetchAction =
      case fetchesAllWithPathsDict (Proxy @(OutAndDepKeys k)) (Proxy @f) of
        Dict -> computeAndSaveValue 1 cfg key

dependencies :: forall k . TaskKey k => TaskConfig k -> k -> Set (Variant (DepKeys k))
dependencies cfg key = toVariants . tailF $ outAndDependencies cfg key

outKeys :: forall k . TaskKey k => TaskConfig k -> k -> Set (OutKey k)
outKeys cfg key = headF $ outAndDependencies cfg key

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
      case fetchesAllWithPathsDict (Proxy @(OutAndDepKeys k)) (Proxy @f) of
        Dict -> computeAndSaveValue numCpus task.config task.key

-- | A stock summary of the dependencies' files (see 'FromInputFiles').
summarizeInputFiles :: FromInputFiles s => [(Variant ks, InputFile)] -> s
summarizeInputFiles = foldMap (fromInputFile . snd)

-- | A task's shape. The traversal of the dependencies and the projections of
-- the stat keys are bound outside 'estimate', so that every estimation of one
-- shape shares them.
taskShapeOf
  :: forall r k
   . (TaskKey k, PathResolver r (OutKey k), All (ToTaskFile r) (DepKeys k))
  => Task r k -> TaskShape
taskShapeOf t = MkTaskShape
  { inputFiles   = Set.fromList (map snd ownDeps)
  , outputFiles  = Set.fromList (map fst outputs)
  , statKey      = encodeStatKey <$> statKey
  , model        = statKey <&> \key -> MkModel
      { decode     = decodeSummary
      , estimateAt = modelFor key
      , isClose    = closeInputSummaries key
      }
  , outputModels = Map.fromList
      [ (file.path, MkModel
          { decode     = decodeSummary
          , estimateAt = fileSizeEstimate fileKey
          , isClose    = closeProducerSummaries fileKey
          })
      | (file, Just fileKey) <- outputs
      ]
  , estimate     = estimate
  }
  where
    outsAndDeps = outAndDependencies t.config t.key
    outputs =
      [ (toTaskFile t.resolver o, fileStatKeyOf o)
      | o <- Set.toList (headF outsAndDeps)
      ]
    ownDeps =
      [ (dep, vAll @(ToTaskFile r) (toTaskFile t.resolver) dep)
      | dep <- Set.toList (toVariants (tailF outsAndDeps))
      ]
    statKey = toStatKey t.config t.key
    modelFor key s = MkResourceEstimates
      { memory  = EstimatedByTask (memoryEstimate key s)
      , runtime = EstimatedByTask (runtimeEstimate key s)
      }
    outputSize s = maybe 0 (`fileSizeEstimate` s)

    estimate inputInfos = MkTaskEstimation
      { inputs          = Set.fromList (map snd deps)
      , outputs         = Set.fromList
          [ withSize (EstimatedByTask (outputSize producerSummary fileKey)) file
          | (file, fileKey) <- outputs
          ]
      , estimates       =
          maybe (estimatesFromModel 0) (`modelFor` inputSummary) statKey
      , inputSummary    = statKey $> encodeSummary inputSummary
      , producerSummary = if any (isJust . snd) outputs
          then Just (encodeSummary producerSummary)
          else Nothing
      }
      where
        deps = [ (dep, inputInfos file) | (dep, file) <- ownDeps ]
        depInputs = [ (dep, toInputFile info) | (dep, info) <- deps ]
        inputSummary = toInputSummary t.config t.key depInputs
        producerSummary = toProducerSummary t.config t.key depInputs

instance
  ( Static(PathResolverForAll r (DepKeys k))
  , All (ToTaskFile r) (DepKeys k)
  , TaskKey k
  , ToJSON (Task r k)
  , Eq (Task r k)
  , Ord (Task r k)
  , All (ToTaskFile r) (DepKeys k)
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
  -- Estimates always come from the stat key, so that the value used for
  -- scheduling is the same function that is validated against recorded
  -- statistics for that key.
  -- A task with no stat key estimates zero memory, hence (via
  -- 'estimatesFromModel') zero runtime.
  taskShape              = taskShapeOf
  taskMaxThreads stage t = maxThreads stage t.config t.key
  taskMinThreads stage t = minThreads stage t.config t.key
  taskDefaultPriority t  = priority t.key
  taskTag t              = tag t.key
  -- A closure-less task completes instantly without a worker round-trip
  -- (see 'Hyperion.Scheduler.RunTasks.RemoteRunTask.remoteRunTask'), which is
  -- all a NoOpTask needs.
  taskClosure t  = case taskKind @k of
    NoOpTask _ -> Nothing
    _ -> Just $ \numCpus -> static computeAndWrite
      `cAp` closureDict
      `cAp` cPure numCpus
      `cAp` cPure t
  taskIsPlaceholder _ = case taskKind @k of
    PlaceholderTask -> True
    _               -> False
  taskPlaceholderKey t = case taskKind @k of
    PlaceholderTask -> cast t.key
    _               -> Nothing

taskLink
  :: forall r c k m
   . ( MonadTaskFiles m, HasConfig c (TaskConfig k), TaskKey k
     , PathResolver r (OutKey k) )
  => r
  -> c
  -> TaskLink m k (Variant (DepKeys k)) (Task r k)
taskLink resolver cfg' = MkTaskLink
  { dependencies = dependencies @k cfg
  , checkCreated = checkCreated cfg resolver
  , toTask = MkTask resolver cfg
  }
  where
    cfg = toConfig cfg'

instance {-# OVERLAPPABLE #-}
  ( Static (TaskKey k)
  , HasTaskChain m r c (Variant (DepKeys k))
  , MonadTaskFiles m
  , HasConfig c (TaskConfig k)
  , Static (PathResolver r (OutKey k))
  , Static (PathResolverForAll r (DepKeys k))
  , All (ToTaskFile r) (DepKeys k)
  , Static (Binary r)
  , Static (Binary k)
  , IsTask (Task r k)
  ) => HasTaskChain m r c k where
  taskChain resolver cfg = TaskNode (wrapTask <$> taskLink resolver cfg) (taskChain resolver cfg)


data ListTaskKey k = MkListTaskKey
  { tag  :: Maybe Text
  , keys :: [k]
  }
  deriving (Generic, Eq, Ord, Show, Binary, ToJSON)

-- Create ListTaskKey with default tag = "ListTaskKey MyKey" (for k ~ MyKey)
-- Use 'MkListTaskKey myTag myKeys' or 'listTaskKey myKeys { tag = myTag }' to override it.
listTaskKey :: forall k. Typeable k => [k] -> ListTaskKey k
listTaskKey = MkListTaskKey $ defaultTaskTagForType @(ListTaskKey k)

type instance DepKeys (ListTaskKey k) = '[k]
instance (Ord k, TaskKey k, ToFileStatKey k) => TaskKey (ListTaskKey k) where
  type OutKey (ListTaskKey k) = Void

  -- A pure grouping node: it performs no computation, so it is never scheduled
  -- by estimate and has nothing to record. That is the default 'StatKeyOf'
  -- ('Void') and the default 'toStatKey' ('Nothing'), so neither is declared.
  taskKind = NoOpTask $ \t -> traverse_ getPath t.keys
  tag t = t.tag

instance (Typeable k , Static(Binary k)) => Static (Binary (ListTaskKey k)) where
  closureDict = static (\Dict -> Dict) `cAp` closureDict @(Binary k)
instance (Static(Ord k), Static(TaskKey k), Static(ToFileStatKey k)) => Static (TaskKey (ListTaskKey k)) where
  closureDict = static (\Dict -> Dict) `cAp` closureDict @(Ord k, TaskKey k, ToFileStatKey k)
instance (Typeable k , Static(Ord k)) => Static (Ord (ListTaskKey k)) where
  closureDict = static (\Dict -> Dict) `cAp` closureDict @(Ord k)
