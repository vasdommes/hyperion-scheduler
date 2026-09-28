{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Where a 'Hyperion.Scheduler.RunTasks.runTasks' spends its time: named
-- timers around the steps that could serialize scheduling, and a timeline of
-- queue length and resource use.
--
-- A timer's name says what it covers; see its call site in
-- "Hyperion.Scheduler.RunTasks". A timer with zero total time is a counter.
module Hyperion.Scheduler.RunTasks.SchedulerStats
  ( SchedulerStats (..)
  , TimerStats (..)
  , Sample (..)
  , Instruments (..)
  , newInstruments
  , Timers
  , newTimers
  , recordTime
  , countEvent
  , timed
  , readTimers
  , Gauge
  , newGauge
  , withGauge
  , incrGauge
  , readGauge
  , getSeconds
  ) where

import Control.Monad.Catch    (MonadMask, bracket_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson             (ToJSON)
import Data.IORef             (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict        (Map)
import Data.Map.Strict        qualified as Map
import Data.Text              (Text)
import GHC.Clock              (getMonotonicTime)
import GHC.Generics           (Generic)

-- | What one 'runTasks' measured.
data SchedulerStats = MkSchedulerStats
  { wallSeconds :: Double
  , timers      :: Map Text TimerStats
  , samples     :: [Sample]
  } deriving (Show, Generic, ToJSON)

data TimerStats = MkTimerStats
  { count        :: !Int
  , totalSeconds :: !Double
  , maxSeconds   :: !Double
  } deriving (Show, Generic, ToJSON)

instance Semigroup TimerStats where
  a <> b = MkTimerStats
    { count        = a.count + b.count
    , totalSeconds = a.totalSeconds + b.totalSeconds
    , maxSeconds   = max a.maxSeconds b.maxSeconds
    }

-- | The scheduler's state at one moment.
data Sample = MkSample
  { seconds     :: !Double -- ^ since 'runTasks' started
  , queueLength :: !Int    -- ^ tasks ready to run, not yet dispatched
  , cpusInUse   :: !Int    -- ^ CPUs allocated to tasks, over all nodes
  , inFlight    :: !Int    -- ^ tasks between dispatch and completion
  , running     :: !Int    -- ^ tasks inside 'remoteRunTask'
  , finished    :: !Int
  } deriving (Show, Generic, ToJSON)

-- | What the scheduler's threads record into.
data Instruments = MkInstruments
  { timers   :: Timers
  , inFlight :: Gauge -- ^ see 'Sample'
  , running  :: Gauge
  , finished :: Gauge
  }

newInstruments :: MonadIO m => m Instruments
newInstruments = MkInstruments <$> newTimers <*> newGauge <*> newGauge <*> newGauge

newtype Timers = MkTimers (IORef (Map Text TimerStats))

newTimers :: MonadIO m => m Timers
newTimers = liftIO $ MkTimers <$> newIORef Map.empty

recordTime :: MonadIO m => Timers -> Text -> Double -> m ()
recordTime (MkTimers ref) name t = liftIO $ atomicModifyIORef' ref $ \m ->
  (Map.insertWith (<>) name (MkTimerStats 1 t t) m, ())

countEvent :: MonadIO m => Timers -> Text -> m ()
countEvent timers name = recordTime timers name 0

-- | Time an action. Nothing is recorded if it throws.
timed :: MonadIO m => Timers -> Text -> m a -> m a
timed timers name go = do
  start <- getSeconds
  x <- go
  end <- getSeconds
  recordTime timers name (end - start)
  pure x

readTimers :: MonadIO m => Timers -> m (Map Text TimerStats)
readTimers (MkTimers ref) = liftIO $ readIORef ref

-- | A number of things currently in some state.
newtype Gauge = MkGauge (IORef Int)

newGauge :: MonadIO m => m Gauge
newGauge = liftIO $ MkGauge <$> newIORef 0

withGauge :: (MonadIO m, MonadMask m) => Gauge -> m a -> m a
withGauge g = bracket_ (addGauge g 1) (addGauge g (-1))

incrGauge :: MonadIO m => Gauge -> m ()
incrGauge g = addGauge g 1

addGauge :: MonadIO m => Gauge -> Int -> m ()
addGauge (MkGauge ref) d = liftIO $ atomicModifyIORef' ref $ \n -> (n + d, ())

readGauge :: MonadIO m => Gauge -> m Int
readGauge (MkGauge ref) = liftIO $ readIORef ref

-- | Monotonic time in seconds.
getSeconds :: MonadIO m => m Double
getSeconds = liftIO getMonotonicTime
