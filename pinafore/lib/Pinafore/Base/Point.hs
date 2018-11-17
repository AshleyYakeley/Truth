module Pinafore.Base.Point where

import Crypto.Hash
import Data.Aeson (FromJSON)
import Data.ByteArray (convert)
import Data.Serialize as Serialize (Serialize(..), encode)
import Data.UUID hiding (fromString)
import Shapes

newtype Anchor =
    MkAnchor UUID
    deriving (Eq, Random, FromJSON)

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

newtype Point =
    MkPoint Anchor
    deriving (Eq, Random, FromJSON, Show, Serialize)

newPoint :: MonadIO m => m Point
newPoint = liftIO randomIO

hashToPoint :: (forall r. (forall t. Serialize t => t -> r) -> [r]) -> Point
hashToPoint f = MkPoint $ hashToAnchor f
