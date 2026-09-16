{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | The wire form of a follow-up: which of the key types a task declares
-- ('Hyperion.Scheduler.Task.Task.FollowUps') and the encoded key. A running
-- task sends it ('Hyperion.Scheduler.Dynamic.addFollowUp') and the scheduler
-- decodes it where the task was built, with the task's own resolver and
-- configs ('Hyperion.Scheduler.Task.IsTask.taskFollowUps').
module Hyperion.Scheduler.Task.FollowUps
  ( BinaryVariant (..)
  , encodeFollowUp
  , decodeFollowUp
  , hasFollowUps
  ) where

import Bootstrap.Build        (Variant (..))
import Data.Binary            (Binary)
import Data.Binary            qualified as Binary
import Data.ByteString.Lazy   (ByteString)
import Data.Proxy             (Proxy (..))

-- | A 'Variant' over 'Binary' types, as an index into the list and the
-- encoded value.
class BinaryVariant ks where
  encodeVariant   :: Variant ks -> (Int, ByteString)
  decodeVariantAt :: Int -> ByteString -> Either String (Variant ks)
  variantCount    :: Proxy ks -> Int

instance BinaryVariant '[] where
  encodeVariant v = case v of {}
  decodeVariantAt i _ = Left ("follow-up index out of range: " <> show i)
  variantCount _ = 0

instance (Binary k, BinaryVariant ks) => BinaryVariant (k ': ks) where
  encodeVariant (VLeft x)  = (0, Binary.encode x)
  encodeVariant (VRight v) = let (i, bytes) = encodeVariant v in (i + 1, bytes)
  decodeVariantAt 0 bytes = case Binary.decodeOrFail bytes of
    Left (_, _, err) -> Left ("could not decode the follow-up key: " <> err)
    Right (_, _, x)  -> Right (VLeft x)
  decodeVariantAt i bytes = VRight <$> decodeVariantAt (i - 1) bytes
  variantCount _ = 1 + variantCount (Proxy @ks)

encodeFollowUp :: BinaryVariant ks => Variant ks -> ByteString
encodeFollowUp = Binary.encode . encodeVariant

decodeFollowUp :: BinaryVariant ks => ByteString -> Either String (Variant ks)
decodeFollowUp bytes = case Binary.decodeOrFail bytes of
  Left (_, _, err)           -> Left ("could not decode the follow-up: " <> err)
  Right (_, _, (i, payload)) -> decodeVariantAt i payload

-- | Whether the list of declared follow-ups is non-empty.
hasFollowUps :: forall ks . BinaryVariant ks => Proxy ks -> Bool
hasFollowUps p = variantCount p > 0
