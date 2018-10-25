module Pinafore.Point where

import Crypto.Hash
import Data.Aeson (FromJSON)
import Data.ByteArray (convert)
import Data.Serialize as Serialize (Serialize(..), encode)
import Data.UUID hiding (fromString)
import Shapes

newtype Point =
    MkPoint UUID
    deriving (Eq, Random, FromJSON)

instance Show Point where
    show (MkPoint uuid) = '!' : show uuid

instance Serialize Point where
    put (MkPoint uuid) = Serialize.put (toByteString uuid)
    get = do
        bs <- Serialize.get
        case fromByteString bs of
            Just uuid -> return $ MkPoint uuid
            Nothing -> fail "deserialize bad UUID"

newPoint :: MonadIO m => m Point
newPoint = liftIO randomIO

hashToPoint :: (forall r. (forall t. Serialize t => t -> r) -> [r]) -> Point
hashToPoint f =
    MkPoint $
    fromMaybe (error "hash failure") $
    fromByteString $
    fromStrict $ take 16 $ convert $ hashFinalize $ hashUpdates (hashInit @SHA3_256) $ f Serialize.encode
