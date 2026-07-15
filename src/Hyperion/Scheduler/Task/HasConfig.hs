{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Hyperion.Scheduler.Task.HasConfig where
import Bootstrap.Build       (All, HasIndex (..), KnownLength (..), Index (..),
                              Length (..))
import Data.Aeson            (ToJSON, Value)
import Data.Aeson.Types      (ToJSON (..))
import Data.Binary           (Binary (..))
import Data.Kind             (Type)

class HasConfig a b where
  toConfig :: a -> b

instance HasConfig a () where
  toConfig = const ()

-- | A heterogeneous list for a collection of Config's. Automatically has a
-- HasConfig constraint for each element.
data Configs (cs :: [Type]) where
  CNil  :: Configs '[]
  CCons :: c -> Configs cs -> Configs (c ': cs)

deriving instance All Show cs => Show (Configs cs)
deriving instance All Eq cs => Eq (Configs cs)
deriving instance (All Eq cs, All Ord cs) => Ord (Configs cs)

instance (KnownLength cs, All Binary cs) => Binary (Configs cs) where
  put xs = case (xs, knownLength @cs) of
    (CNil, LZero)          -> pure ()
    (CCons x xs', LSucc _) -> put x >> put xs'
  get = case knownLength @cs of
    LZero -> pure CNil
    LSucc (_ :: Length cs') -> do
      x <- get
      fmap (CCons x) (get @(Configs cs'))

instance All ToJSON cs => ToJSON (Configs cs) where
  toJSON = toJSON . go
    where
      go :: All ToJSON cs' => Configs cs' -> [Value]
      go CNil         = []
      go (CCons x xs) = toJSON x : go xs

instance {-# OVERLAPPING #-} HasConfig (Configs cs) () where
  toConfig = const ()

instance {-# OVERLAPPABLE #-} HasIndex c cs => HasConfig (Configs cs) c where
  toConfig = go (index @c @cs)
    where
      go :: Index a as -> Configs as -> a
      go Here (CCons cfg _)        = cfg
      go (There i') (CCons _ cfgs) = go i' cfgs
