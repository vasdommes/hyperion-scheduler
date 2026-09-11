{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE OverloadedRecordDot     #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE StaticPointers          #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | PathResolver is a typeclass for transforming task keys into paths.
-- It generalizes the old BoundFiles + toPath approach.
module Hyperion.Scheduler.PathResolver where

import Bootstrap.Build (All, AllIn, HasIndex (..), KnownLength (..),
                        Length (..), Variant, allInSelfDict, toVariantAt)
import Data.Kind       (Constraint)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Typeable   (Typeable)
import Data.Void       (Void, absurd)
import Hyperion        (Dict (..), Static (..))
import Hyperion.OsPath (OsPath)

-- Resolver r returns output file path produced by Task a.
class PathResolver r a where
  resolvePath :: r -> a -> OsPath

type family PathResolverForAll r xs :: Constraint where
  PathResolverForAll r '[] = ()
  PathResolverForAll r (x ': xs) = (PathResolver r x, PathResolverForAll r xs)

-- Void can be used e.g. by TaskKey instances as
-- type (OutKey k) = Void
-- to indicate that the task does not produce any files.
instance PathResolver r Void where
  resolvePath _ = absurd


instance Typeable r => Static (PathResolver r Void) where
  closureDict = static Dict

-- | Resolve paths from a precomputed map, e.g. of a task's
-- dependencies (cf. 'Hyperion.Scheduler.Task.Task.getPathVariant').
newtype MapResolver ks = MkMapResolver (Map (Variant ks) OsPath)

instance (All Eq ks, All Ord ks, HasIndex k ks) => PathResolver (MapResolver ks) k where
  resolvePath (MkMapResolver m) key = case Map.lookup (toVariantAt (index @k @ks) key) m of
    Just path -> path
    -- TODO: shall resolvePath return (Maybe OsPath)?
    Nothing   -> error "MapResolver doesn't contain expected key"

-- | A 'MapResolver' over a key list resolves every key in that list:
-- the 'HasIndex k ks' needed per key is a tautology, proved by
-- 'allInSelfDict'.
mapResolverForAllDict
  :: forall ks . (All Eq ks, All Ord ks, KnownLength ks)
  => Dict (PathResolverForAll (MapResolver ks) ks)
mapResolverForAllDict = case allInSelfDict (knownLength @ks) of
  Dict -> go (knownLength @ks)
  where
    go
      :: forall ks' . AllIn ks' ks
      => Length ks'
      -> Dict (PathResolverForAll (MapResolver ks) ks')
    go LZero = Dict
    go (LSucc l) = case go l of
      Dict -> Dict
