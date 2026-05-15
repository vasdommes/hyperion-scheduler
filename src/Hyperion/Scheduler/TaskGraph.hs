{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Hyperion.Scheduler.TaskGraph where

import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Array           qualified as Array
import Data.Foldable        (foldrM)
import Data.Graph           (Graph)
import Data.Graph           qualified as Graph
import Data.IntMap.Strict   (IntMap)
import Data.IntMap.Strict   qualified as IntMap
import Data.Map.Strict      (Map)
import Data.Map.Strict      qualified as Map
import Data.Set             (Set)
import Data.Set             qualified as Set
import Data.Tree            (Tree)
import Data.Tree            qualified as Tree
import Data.Vector          (Vector)
import Data.Vector          qualified as Vector

data TaskGraph a = MkTaskGraph
  { dependencyGraph    :: Graph
  , revDependencyGraph :: Graph
  , tasks              :: Vector a
  , vertexMap          :: Map a Graph.Vertex
  -- The longest path from a task to its descendants
  , depths             :: Map a Int
  -- The longest path from a task to its dependencies
  , revDepths          :: Map a Int
  } deriving (Eq, Ord, Show)

-- | Given a Tree, tabulate the pairs of (node, children). This
-- function assumes that a given node always has the same children, no
-- matter where or how many times it appears in the tree. If this is
-- not the case, then only one of the instances of the node will be
-- used.
treeToEdges :: Ord a => Tree a -> Map a (Set a)
treeToEdges t = go t Map.empty
  where
    go (Tree.Node x xs) m =
      Map.insert x (Set.fromList (map Tree.rootLabel xs)) $
      foldr go m xs

fromEdges :: Ord a => Map a (Set a) -> TaskGraph a
fromEdges edges = MkTaskGraph
  { dependencyGraph    = depGraph
  , revDependencyGraph = revDepGraph
  , tasks              = Vector.fromList $ map vertexToKey vertices
  , vertexMap          = Map.fromList [(vertexToKey v, v) | v <- vertices]
  , depths             = toDepths depGraph
  , revDepths          = toDepths revDepGraph
  }
  where
    (depGraph, nodeFromVertex, _) =
      Graph.graphFromEdges [(k, k, Set.toList ks) | (k, ks) <- Map.toList edges]
    revDepGraph = Graph.transposeG depGraph
    vertexToKey v = case nodeFromVertex v of
      (k, _, _) -> k
    vertices = Graph.vertices depGraph
    toDepths graph = Map.fromList $ do
      (v, d) <- IntMap.toList (graphToDepthMap graph)
      pure (vertexToKey v, d)


fromTree :: Ord a => Tree a -> TaskGraph a
fromTree = fromEdges . treeToEdges

taskToVertex :: Ord a => TaskGraph a -> a -> Graph.Vertex
taskToVertex graph t = graph.vertexMap Map.! t

vertexToTask :: TaskGraph a -> Graph.Vertex -> a
vertexToTask graph v = graph.tasks Vector.! v

dependencies :: Ord a => TaskGraph a -> a -> Set a
dependencies graph t =
  Set.fromList $
  map (vertexToTask graph) $
  graph.dependencyGraph Array.! taskToVertex graph t

reverseDependencies :: Ord a => TaskGraph a -> a -> Set a
reverseDependencies graph t =
  Set.fromList $
  map (vertexToTask graph) $
  graph.revDependencyGraph Array.! taskToVertex graph t

dependencyCounts :: Ord a => TaskGraph a -> Map a Int
dependencyCounts graph = Map.fromList $ do
  t <- Vector.toList graph.tasks
  pure (t, length (dependencies graph t))

-- | Given a directed acyclic graph, return a 'Map' from vertices to
-- "depth"s, where depth is the length of the longest path through the
-- graph that ends at the vertex. For example, if a vertex has no
-- incoming edges, its depth will be 0. If a vertex has an incoming
-- edge from a parent vertex v' with depth d', then its depth is at
-- least d' + 1 (it might be more if there is a longer path coming
-- through a different parent).
graphToDepthMap :: Graph.Graph -> IntMap Int
graphToDepthMap graph = go (Graph.topSort graph) IntMap.empty
  where
    go [] m = m
    go (v : vs) m =
      let
        vDepth = IntMap.findWithDefault 0 v m
        updateDep Nothing  = Just (vDepth + 1)
        updateDep (Just d) = Just (max d (vDepth + 1))
        m' =
          IntMap.insert v vDepth $
          foldr (IntMap.alter updateDep) m (graph Array.! v)
      in go vs m'

-- | All tasks in a TaskGraph.
keys :: TaskGraph a -> Set a
keys graph = Map.keysSet graph.vertexMap

-- | Keys with no dependencies.
independentKeys :: Ord a => TaskGraph a -> Set a
independentKeys = Map.keysSet . Map.filter (== 0) . dependencyCounts

type DepCounts a = Map a Int

-- TODO: We treat independent tasks differently from the others: they
-- are not removed from the DepCount's, whereas others are. We should
-- probably treat them the same way.

-- | Decrement a tasks's dependency count. When the count reaches 0,
-- remove the task from the DepCounts and call 'tell' on it.
decrementDepCount :: Ord a => a -> DepCounts a -> Writer [a] (DepCounts a)
decrementDepCount task = Map.alterF decr task
  where
    decr Nothing = error "decrementDepCount: missing element"
    decr (Just n)
      | n > 1     = pure (Just (n-1))
      | otherwise = tell [task] >> pure Nothing

-- | Given a task, lookup its reverse dependencies, and decrement
-- their dependency counts. Return any tasks whose dependency count
-- reached 0 (and were thus removed from the DepCounts).
decrementReverseDependencies
  :: Ord a
  => TaskGraph a
  -> DepCounts a
  -> a
  -> (DepCounts a, [a])
decrementReverseDependencies graph depCounts task =
  runWriter $
  foldrM decrementDepCount depCounts $
  reverseDependencies graph task
