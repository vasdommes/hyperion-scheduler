{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE StaticPointers        #-}
{-# LANGUAGE UndecidableInstances  #-}

module Hyperion.Scheduler.TaskLink where

import Bootstrap.Build.FList    (Variant (..))
import Data.Maybe               (catMaybes)
import Data.Set                 (Set)
import Data.Set                 qualified as Set
import Data.Tree                (Tree)
import Data.Tree                qualified as Tree
import Data.Void                (Void)
import Hyperion.Scheduler.Stats (TaskAndFileStats)

data TaskLink m k d t = MkTaskLink
  { dependencies :: k -> Set d
  , checkCreated :: k -> m Bool
  , toTask       :: k -> t
  } deriving (Functor)

data TaskChain m k t where
  TaskNil   :: TaskChain m Void t
  TaskNode  :: TaskLink m k d t -> TaskChain m d t -> TaskChain m k t
  TaskMerge :: TaskChain m k t -> TaskChain m (Variant ks) t -> TaskChain m (Variant (k ': ks)) t

class HasTaskChain m r k t where
  taskChain :: TaskAndFileStats -> r -> TaskChain m k t

instance HasTaskChain m r (Variant '[]) t where
  taskChain _ _ = error "absurd"

instance ( HasTaskChain m r k t
         , HasTaskChain m r (Variant ks) t
         ) => HasTaskChain m r (Variant (k ': ks)) t where
  taskChain stats x = TaskMerge (taskChain stats x) (taskChain stats x)

-- | Turn a TaskChain into a tree of tasks
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
