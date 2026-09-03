{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StaticPointers        #-}
{-# LANGUAGE UndecidableInstances  #-}

module Hyperion.Scheduler.Task.TaskLink where

import Bootstrap.Build                     (Variant (..))
import Control.Monad.State.Strict          (StateT, execStateT, gets, lift,
                                            modify')
import Data.Aeson                          (ToJSON (..))
import Data.Binary                         (Binary)
import Data.Map.Strict                     (Map)
import Data.Map.Strict                     qualified as Map
import Data.Maybe                          (catMaybes)
import Data.Set                            (Set)
import Data.Set                            qualified as Set
import Data.Tree                           (Tree)
import Data.Tree                           qualified as Tree
import Data.Typeable                       (Typeable)
import Data.Void                           (Void)
import Hyperion.Scheduler.StatKey          (ToStatKey (..))
import Hyperion.Scheduler.Task.IsTask      (IsTask (..))
import Hyperion.Scheduler.Task.WrappedTask (WrappedTask, wrapTask)

data TaskLink m k d t = MkTaskLink
  { dependencies :: k -> Set d
  , checkCreated :: k -> m Bool
  , toTask       :: k -> t
  } deriving (Functor)

emptyTaskLink :: Applicative m => TaskLink m (Variant '[]) Void t
emptyTaskLink = MkTaskLink
  { dependencies = const Set.empty
  , checkCreated = const (pure True)
  , toTask       = error "absurd"
  }

data TaskChain m k t where
  TaskNil   :: TaskChain m Void t
  TaskNode  :: TaskLink m k d t -> TaskChain m d t -> TaskChain m k t
  TaskMerge :: TaskChain m k t -> TaskChain m (Variant ks) t -> TaskChain m (Variant (k ': ks)) t

emptyTaskChain :: Applicative m => TaskChain m (Variant '[]) t
emptyTaskChain = TaskNode emptyTaskLink TaskNil

class HasTaskChain m r c k where
  taskChain :: r -> c -> TaskChain m k WrappedTask

instance Applicative m => HasTaskChain m r c (Variant '[]) where
  taskChain _ _ = emptyTaskChain

instance (HasTaskChain m r c k, HasTaskChain m r c (Variant ks)) => HasTaskChain m r c (Variant (k ': ks)) where
  taskChain resolver cfg = TaskMerge (taskChain resolver cfg) (taskChain resolver cfg)

newtype ListTask k = MkListTask { keys :: [k] }
  deriving newtype (Binary, ToJSON, Eq, Ord)

instance (Eq k, Ord k, ToJSON k, Typeable k) => IsTask (ListTask k) where
  taskMaxThreads _ _ = 0
  taskMinThreads _ _ = 0
  taskInputs _       = Set.empty
  taskOutputs _      = Set.empty
  taskTag _          = Nothing
  taskClosure _ _    = Nothing

instance ToStatKey (ListTask k) where
  toStatKey _ = toStatKey ()

listTaskLink :: (Ord k, Applicative m) => TaskLink m [k] k (ListTask k)
listTaskLink = MkTaskLink
  { dependencies = Set.fromList
  , checkCreated = const (pure False)
  , toTask = MkListTask
  }

instance (Ord k, Typeable k, Binary k, ToJSON k, Applicative m, HasTaskChain m r c k) => HasTaskChain m r c [k] where
  taskChain resolver cfg = TaskNode (wrapTask <$> listTaskLink) (taskChain resolver cfg)

instance (Ord k, Typeable k, Binary k, ToJSON k, Applicative m, HasTaskChain m r c k) => HasTaskChain m r c (Set k) where
  taskChain resolver cfg = TaskNode (wrapTask <$> contramapKey Set.toList listTaskLink) (taskChain resolver cfg)

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
  , HasTaskChain m r c k
  , HasTaskChain m r c k'
  ) => HasTaskChain m r c (k, k') where
  taskChain resolver cfg = TaskNode (wrapTask <$> pairTaskLink) (taskChain resolver cfg)

contramapKey :: (k' -> k) -> TaskLink m k d t -> TaskLink m k' d t
contramapKey f taskLink =
  MkTaskLink
    { dependencies = taskLink.dependencies . f
    , checkCreated = taskLink.checkCreated . f
    , toTask       = taskLink.toTask . f
    }

-- | Turn a TaskChain into a tree of tasks. Don't use this function!
{-# DEPRECATED toTaskTree "This function is slow, use Hyperion.Scheduler.Task.TaskMap.toTaskEdges instead!" #-}
toTaskTree :: Monad m => TaskChain m k t -> k -> m (Maybe (Tree t))
toTaskTree TaskNil _ = error "absurd"
toTaskTree (TaskNode link chain) key = do
  created <- link.checkCreated key
  if created
    then pure Nothing
    else do
    deps <- mapM (toTaskTree chain) (Set.toList (link.dependencies key))
    pure $ Just $ Tree.Node (link.toTask key) (catMaybes deps)
toTaskTree (TaskMerge c1 c2) key = case key of
  VLeft k   -> toTaskTree c1 k
  VRight ks -> toTaskTree c2 ks

-- | Turn a TaskChain into a map from tasks to Sets of dependencies. This is
-- preferred over toTaskTree for performance reasons.
toTaskEdges :: forall m k t . (Monad m, Ord t) => TaskChain m k t -> k -> m (Map t (Set t))
toTaskEdges chain initialKey = execStateT (go chain initialKey) Map.empty
  where
    -- (Just task) indicates that the task needs to be done. Nothing indicates
    -- that it does not.
    go :: forall k' . TaskChain m k' t -> k' -> StateT (Map t (Set t)) m (Maybe t)
    go TaskNil _ = error "absurd"
    go (TaskNode link rest) key = do
      let task = link.toTask key
      explored <- gets (Map.member task)
      if explored
        then pure (Just task)
        else do
          created <- lift $ link.checkCreated key
          if created
            -- Task already completed, does not need to be added to the graph
            then pure Nothing
            else do
              -- Explore the dependencies and insert the ones that need to be done
              maybeDeps <- mapM (go rest) (Set.toList (link.dependencies key))
              let deps = Set.fromList $ catMaybes maybeDeps
              modify' (Map.insert task deps)
              pure $ Just task
    go (TaskMerge c1 c2) key = case key of
      VLeft k   -> go c1 k
      VRight ks -> go c2 ks
