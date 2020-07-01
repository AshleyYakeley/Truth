module Data.Codec where

import Data.CatFunctor
import Data.Isomorphism
import Data.MonadOne
import Data.Result
import qualified Data.Serialize as Serialize
import Shapes.Import

class IsBiMap bm where
    mapBiMapM :: (forall x. m1 x -> m2 x) -> bm m1 a b -> bm m2 a b

toBiMapMaybe :: (IsBiMap bm, MonadOne m) => bm m edita editb -> bm Maybe edita editb
toBiMapMaybe = mapBiMapM getMaybeOne

toBiMapResult ::
       forall e bm m edita editb. (IsBiMap bm, MonadOne m)
    => e
    -> bm m edita editb
    -> bm (Result e) edita editb
toBiMapResult e = mapBiMapM (mrf . retrieveOne)
  where
    mrf :: Result (m None) a -> Result e a
    mrf (SuccessResult a) = SuccessResult a
    mrf (FailureResult _) = FailureResult e

data Codec' m a b = MkCodec
    { decode :: a -> m b
    , encode :: b -> a
    }
    -- must have decode . encode = Just

decodeMaybe :: MonadOne m => Codec' m a b -> a -> Maybe b
decodeMaybe codec = getMaybeOne . decode codec

instance IsBiMap Codec' where
    mapBiMapM ff codec = MkCodec {decode = ff . (decode codec), encode = encode codec}

type Codec = Codec' Maybe

instance (Monad m) => Category (Codec' m) where
    id = MkCodec return id
    (MkCodec bmc cb) . (MkCodec amb ba) = MkCodec (\a -> (amb a) >>= bmc) (ba . cb)

bijectionCodec :: Applicative m => Bijection a b -> Codec' m a b
bijectionCodec (MkIsomorphism p q) = MkCodec (pure . p) q

instance (Traversable f, Applicative m) => CatFunctor (Codec' m) (Codec' m) f where
    cfmap codec = MkCodec {decode = traverse (decode codec), encode = fmap (encode codec)}

serializeLazyCodec ::
       forall m t. (MonadFail m, Serialize t)
    => Codec' m LazyByteString t
serializeLazyCodec = let
    encode = Serialize.encodeLazy
    decode = resultToM . eitherToResult . Serialize.decodeLazy
    in MkCodec {..}

serializeStrictCodec ::
       forall m t. (MonadFail m, Serialize t)
    => Codec' m StrictByteString t
serializeStrictCodec = let
    encode = Serialize.encode
    decode = resultToM . eitherToResult . Serialize.decode
    in MkCodec {..}

encodeM :: Codec a b -> b -> a
encodeM = encode
