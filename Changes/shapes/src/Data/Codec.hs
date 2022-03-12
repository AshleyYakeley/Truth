module Data.Codec where

import Control.Monad.Ology.Functor.MonadOne
import Control.Monad.Ology.Result
import Data.CatFunctor
import Data.IsoVariant
import Data.Isomorphism
import Shapes.Import

class IsBiMap bm where
    mapBiMapM :: (forall x. m1 x -> m2 x) -> bm m1 a b -> bm m2 a b

toBiMapMaybe :: (IsBiMap bm, MonadOne m) => bm m edita editb -> bm Maybe edita editb
toBiMapMaybe = mapBiMapM fextractm

toBiMapResult ::
       forall e bm m edita editb. (IsBiMap bm, MonadOne m)
    => e
    -> bm m edita editb
    -> bm (Result e) edita editb
toBiMapResult e = mapBiMapM (mrf . retrieveOne)
  where
    mrf :: Result (m Void) a -> Result e a
    mrf (SuccessResult a) = SuccessResult a
    mrf (FailureResult _) = FailureResult e

data Codec' m a b = MkCodec
    { decode :: a -> m b
    , encode :: b -> a
    }
    -- must have decode . encode = Just

hoistCodec :: (forall x. m1 x -> m2 x) -> Codec' m1 a b -> Codec' m2 a b
hoistCodec f (MkCodec d e) = MkCodec (f . d) e

decodeMaybe :: MonadOne m => Codec' m a b -> a -> Maybe b
decodeMaybe codec = fextractm . decode codec

toCodec :: MonadOne m => Codec' m a b -> Codec a b
toCodec = hoistCodec fextractm

instance Functor m => IsoVariant (Codec' m p) where
    isoMap ab ba (MkCodec d e) = MkCodec (\p -> fmap ab $ d p) (e . ba)

instance IsBiMap Codec' where
    mapBiMapM ff codec = MkCodec {decode = ff . (decode codec), encode = encode codec}

type Codec = Codec' Maybe

instance Monad m => Category (Codec' m) where
    id = MkCodec return id
    (MkCodec bmc cb) . (MkCodec amb ba) = MkCodec (\a -> (amb a) >>= bmc) (ba . cb)

bijectionCodec :: Applicative m => Bijection a b -> Codec' m a b
bijectionCodec (MkIsomorphism p q) = MkCodec (pure . p) q

codecBijection :: Monad m => Codec' m a b -> Bijection (m a) (m b)
codecBijection (MkCodec amb ba) = MkIsomorphism (\ma -> ma >>= amb) (fmap ba)

codecSum :: Summish f => Codec a b -> f b -> f a -> f a
codecSum MkCodec {..} fb fa =
    isoMap
        (either encode id)
        (\a ->
             case decode a of
                 Just b -> Left b
                 Nothing -> Right a) $
    fb <+++> fa

instance (Traversable f, Applicative m) => CatFunctor (Codec' m) (Codec' m) f where
    cfmap codec = MkCodec {decode = traverse (decode codec), encode = fmap (encode codec)}

encodeM :: Codec a b -> b -> a
encodeM = encode

readShowCodec :: (Read a, Show a) => Codec String a
readShowCodec = MkCodec readMaybe show

class CodecMap f where
    codecMap :: forall a b. Codec a b -> f a -> f b
    default codecMap :: MonadPlus f => forall a b. Codec a b -> f a -> f b
    codecMap codec fa = do
        a <- fa
        mpure $ decode codec a

instance CodecMap (Codec p) where
    codecMap = (.)

codecMap' :: (CodecMap f, MonadOne m) => Codec' m a b -> f a -> f b
codecMap' codec = codecMap $ toCodec codec

utf8Codec :: Codec' (Result UnicodeException) StrictByteString Text
utf8Codec = MkCodec (eitherToResult . decodeUtf8') encodeUtf8
