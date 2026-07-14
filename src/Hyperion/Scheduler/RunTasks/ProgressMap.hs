{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Hyperion.Scheduler.RunTasks.ProgressMap where

import Control.Monad.IO.Class         (MonadIO)
import Data.Map.Strict                (Map)
import Data.Map.Strict                qualified as Map
import Data.Maybe                     (catMaybes)
import Data.Set                       (Set)
import Data.Set                       qualified as Set
import Data.Text                      qualified as Text
import Hyperion.Log                   qualified as Log
import Hyperion.Scheduler.Task.IsTask (IsTask (..), Tag)
import Text.Printf                    qualified as Printf

data Progress = MkProgress
  { completed :: Int
  , total     :: Int
  } deriving (Eq, Ord, Show)

type ProgressMap = Map (Maybe Tag) Progress

noProgressYet :: Int -> Progress
noProgressYet n = MkProgress 0 n

-- | Given a set of tasks, make a tally of the number of tasks with
-- each tag, with no completed tasks yet.
fromTasksTodo :: IsTask a => Set a -> ProgressMap
fromTasksTodo tasks =
  fmap noProgressYet $
  Map.fromListWith (+) $ do
  t <- Set.toList tasks
  pure (taskTag t, 1)

-- | Update a Progress Map to account for a finished task
update :: IsTask a => a -> ProgressMap -> ProgressMap
update task =
  Map.adjust (\p -> p { completed = p.completed + 1 }) (taskTag task)

-- | Determine if all tasks have been completed
isFinished :: ProgressMap -> Bool
isFinished = all (\p -> p.completed == p.total)

displayLog :: MonadIO m => ProgressMap -> m ()
displayLog progressMap = do
  let
    tags = catMaybes $ Map.keys progressMap
    maxLength = maximum (map Text.length tags)
    reports = Text.intercalate "\n" $ do
      (Just tag, prog) <- Map.toList progressMap
      pure $ Text.concat
        [ tag
        , ": "
        , Text.replicate (maxLength - Text.length tag) " "
        , Log.showText (progressBar prog.completed prog.total)
        ]
  Log.text $ "Progress:\n" <> reports

---------- ProgressBar, from Hyperion.Util.MapMontored. TODO: Don't duplicate code.

data ProgressBar = MkProgressBar Rational Rational

progressBar :: Real a => a -> a -> ProgressBar
progressBar x y = MkProgressBar (toRational x) (toRational y)

instance Show ProgressBar where
  show (MkProgressBar n total) =
    "[" ++ addArrow (replicate nEquals '=') ++ replicate nSpaces ' ' ++ "]" ++
    Printf.printf " %d/%d (%.1f%%)" (ceiling @_ @Int n) (ceiling @_ @Int total) percent
    where
      percent = 100 * fromRational @Double (n/total)
      nEquals = ceiling $ toRational len * n/total
      nSpaces = len - nEquals
      len = 40
      addArrow []     = []
      addArrow str@(_:cs)
        | n == total = str
        | otherwise  = reverse $ '>':cs
