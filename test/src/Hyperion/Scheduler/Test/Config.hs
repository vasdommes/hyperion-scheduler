{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StaticPointers        #-}
{-# LANGUAGE TypeFamilies          #-}

module Hyperion.Scheduler.Test.Config where

import Control.Monad.IO.Class    (liftIO)
import Data.Binary               (Binary)
import Data.Maybe                (fromMaybe)
import GHC.Generics              (Generic)
import Hyperion                  (CommandTransport (..), HostNameStrategy (..),
                                  HyperionConfig (..),
                                  HyperionStaticConfig (..), Job,
                                  defaultHyperionConfig,
                                  defaultHyperionStaticConfig)
import Hyperion.OsPath           (OsPath, normalise, (</>))
import Hyperion.OsString         (OsString, fromString, isPrefixOf)
import Hyperion.Scheduler.Config qualified as Scheduler
import Hyperion.Slurm            qualified as Slurm
import System.Directory.OsPath   (makeAbsolute)
import System.Environment        (getEnv)

--data Config = MkConfig
--  { hyperionStaticConfig :: HyperionStaticConfig
--  , getHyperionConfig    :: IO HyperionConfig
--  , getSchedulerConfig   :: Closure (Job Scheduler.Config)
--  }
--
--
--expanseConfig :: Config
--expanseConfig = MkConfig
--  { hyperionStaticConfig = defaultHyperionStaticConfig
--      { hostNameStrategy = GetHostEntriesExternal }
--  , getHyperionConfig = do
--      user <- getEnv "USER"
--      pure $ (defaultHyperionConfig $ "/expanse/lustre/scratch" </> fromString user </> "temp_project")
--        { defaultSbatchOptions = Slurm.defaultSbatchOptions
--          { Slurm.partition = Just "compute"
--          , Slurm.account   = Just "yun124"
--          }
--        , maxSlurmJobs      = Just 64
--        }
--  , getSchedulerConfig = closure $ do
--      user <- liftIO $ getEnv "USER"
--      slurmJobId <- liftIO $ getEnv "SLURM_JOB_ID"
--      let localStoragePath = "/scratch" </> fromString user </> "job_" <> fromString slurmJobId
--      pure $ Scheduler.MkConfig
--        { nodeMemory           = 240 * 1024 * 1024 * 1024
--        , nodeLocalStorageSize = 900 * 1000 * 1000 * 1000
--        , localStoragePath     = localStoragePath
--        , isLocalPath          = isPrefixOf localStoragePath . normalise
--        , reportInterval       = 10
--        }
--  }

type HPCName = OsString

hyperionStaticConfig :: HPCName -> HyperionStaticConfig
hyperionStaticConfig key = case key of
  "expanse" -> defaultHyperionStaticConfig
    { hostNameStrategy = GetHostEntriesExternal
    -- , commandTransport = SRun $ Just ("srun", ["--nodes=1", "--ntasks=1", "--immediate", "--overlap"])
    , nodeLauncherTimeoutRetries = Just (10, 5)
    }
  _         -> defaultHyperionStaticConfig

getScratchDir :: HPCName ->IO OsPath
getScratchDir "expanse" = do
  user <- getEnv "USER"
  pure $ "/expanse/lustre/scratch" </> fromString user </> "temp_project/hyperion-scheduler/test"
getScratchDir _ = undefined

getHyperionConfig :: HPCName -> Maybe OsPath -> IO HyperionConfig
getHyperionConfig key mDir = case key of
  "expanse" -> do
    scratchDir <- getScratchDir key
    workDir <- makeAbsolute $ fromMaybe scratchDir mDir
    pure $ (defaultHyperionConfig workDir)
      { defaultSbatchOptions = Slurm.defaultSbatchOptions
        { Slurm.partition = Just "compute"
        , Slurm.account   = Just "yun124"
        }
      , maxSlurmJobs      = Just 64
      }
  _ -> undefined

getSchedulerConfig :: HPCName -> Job Scheduler.Config
getSchedulerConfig key = case key of
  "expanse" -> do
    user <- liftIO $ getEnv "USER"
    slurmJobId <- liftIO $ getEnv "SLURM_JOB_ID"
    let localStoragePath = "/scratch" </> fromString user </> "job_" <> fromString slurmJobId
    pure $ Scheduler.MkConfig
      { nodeMemory           = 240 * 1024 * 1024 * 1024
      , nodeLocalStorageSize = 900 * 1000 * 1000 * 1000
      , localStoragePath     = localStoragePath
      , isLocalPath          = isPrefixOf localStoragePath . normalise
      , reportInterval       = 10
      }
  _ -> undefined
