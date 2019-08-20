module Pinafore.Base.Entity where

import Control.DeepSeq
import Crypto.Hash
import Data.Aeson (FromJSON)
import Data.ByteArray (convert)
import Data.Serialize as Serialize (Serialize(..), encode)
import Data.UUID hiding (fromString)
import Shapes

newtype Anchor =
    MkAnchor UUID
    deriving (Eq, Ord, Random, FromJSON, NFData)

instance Show Anchor where
    show (MkAnchor uuid) = '!' : show uuid

instance Serialize Anchor where
    put (MkAnchor uuid) = Serialize.put (toByteString uuid)
    get = do
        bs <- Serialize.get
        case fromByteString bs of
            Just uuid -> return $ MkAnchor uuid
            Nothing -> fail "deserialize bad UUID"

hashToAnchor :: (forall r. (forall t. Serialize t => t -> r) -> [r]) -> Anchor
hashToAnchor f =
    MkAnchor $
    fromMaybe (error "hash failure") $
    fromByteString $
    fromStrict $ take 16 $ convert $ hashFinalize $ hashUpdates (hashInit @SHA3_256) $ f Serialize.encode

newtype Entity =
    MkEntity Anchor
    deriving (Eq, Ord, Random, FromJSON, Show, Serialize, NFData)

newEntity :: MonadIO m => m Entity
newEntity = liftIO randomIO

hashToEntity :: (forall r. (forall t. Serialize t => t -> r) -> [r]) -> Entity
hashToEntity f = MkEntity $ hashToAnchor f

maybeToEntity :: Maybe Entity -> Entity
maybeToEntity (Just a) = hashToEntity $ \call -> [call @Text "Just:", call a]
maybeToEntity Nothing = hashToEntity $ \call -> [call @Text "Nothing:"]

pairToEntity :: (Entity, Entity) -> Entity
pairToEntity (a, b) = hashToEntity $ \call -> [call @Text "pair:", call a, call b]

eitherToEntity :: Either Entity Entity -> Entity
eitherToEntity (Left v) = hashToEntity $ \call -> [call @Text "Left:", call v]
eitherToEntity (Right v) = hashToEntity $ \call -> [call @Text "Right:", call v]