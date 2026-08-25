{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE OverloadedRecordDot     #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE StaticPointers          #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | PathResolver is a typeclass for transforming task keys into paths.
-- It generalizes the old BoundFiles + toPath approach.
module Hyperion.Scheduler.PathResolver where

import Data.Kind       (Constraint)
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
