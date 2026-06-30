{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Hyperion.Scheduler.Task.ToTaskGraph where

import Bootstrap.Build.FList       (Variant (..))
import Control.Monad.State.Strict  (StateT, execStateT, gets, lift, modify')
import Data.Map                    qualified as Map
import Data.Map.Strict             (Map)
import Data.Maybe                  (catMaybes)
import Data.Set                    (Set)
import Data.Set                    qualified as Set
import Hyperion.Scheduler.TaskLink (TaskChain (..), TaskLink (..))

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




