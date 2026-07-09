{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Hyperion.Scheduler.RunTasks.NodeStatus where

import Data.IntMap.Strict             (IntMap)
import Data.IntMap.Strict             qualified as IntMap
import Data.List                      (intercalate)
import Data.Text                      (Text)
import Data.Text                      qualified as Text
import Hyperion.Scheduler.Task.IsTask (IsTask (..))
import Hyperion.Scheduler.Types       (MemorySize, NumCPUs)

-- | Currently in-use resources on a node
data NodeStatus = MkNodeStatus
  { cpusInUse    :: NumCPUs    -- ^ CPUs currently in use on a node
  , memoryInUse  :: MemorySize -- ^ Memory currently in use on a node
  , cpuHistogram :: Histogram  -- ^ Histogram of tasks organized by cpu allocation
  } deriving (Eq, Ord, Show)

empty :: NodeStatus
empty = MkNodeStatus 0 0 emptyHistogram

-- | Add a task to NodeStatus
addTask :: IsTask a => (a, NumCPUs) -> NodeStatus -> NodeStatus
addTask (task, taskCpus) status = status
  { cpusInUse    = status.cpusInUse + taskCpus
  , memoryInUse  = status.memoryInUse + taskMemoryEstimate task
  , cpuHistogram = incrHistogram taskCpus status.cpuHistogram
  }

-- | Remove a task from NodeStatus
removeTask :: IsTask a => (a, NumCPUs) -> NodeStatus -> NodeStatus
removeTask (task, taskCpus) status = status
  { cpusInUse    = status.cpusInUse - taskCpus
  , memoryInUse  = status.memoryInUse - taskMemoryEstimate task
  , cpuHistogram = decrHistogram taskCpus status.cpuHistogram
  }

display :: [NodeStatus] -> Text
display statuses =
  Text.intercalate "\n" $
  map (\n -> Text.pack $ displayHistogram n.cpuHistogram) statuses

---------- Histogram ----------

-- | A multi-set of Int's, which we use for visualizing the tasks
-- assigned to a node.
newtype Histogram = MkHistogram (IntMap Int)
  deriving (Eq, Ord, Show)

emptyHistogram :: Histogram
emptyHistogram = MkHistogram IntMap.empty

incrHistogram :: Int -> Histogram -> Histogram
incrHistogram i (MkHistogram h) = MkHistogram $ IntMap.alter incr i h
  where
    incr Nothing  = Just 1
    incr (Just j) = Just (j+1)

decrHistogram :: Int -> Histogram -> Histogram
decrHistogram i (MkHistogram h) = MkHistogram $ IntMap.alter decr i h
  where
    decr Nothing  = error "IntMap: key not found"
    decr (Just 1) = Nothing
    decr (Just j) = Just (j-1)

histogramToList :: Histogram -> [Int]
histogramToList (MkHistogram h) = do
  (k, n) <- IntMap.toList h
  replicate n k

displayHistogram :: Histogram -> String
displayHistogram h =
  case reverse (histogramToList h) of
    [] -> "| 0"
    ns -> "|" <> intercalate "|" [ replicate (n-1) '-' | n <- ns] <> "| " <> show (sum ns)
