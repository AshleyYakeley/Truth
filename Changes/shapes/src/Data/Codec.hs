module Data.Codec where

import Data.CatFunctor
import Data.Isomorphism
import Shapes.Import

class IsBiMap (bm :: (Type -> Type) -> Type -> Type -> Type) where
    mapBiMapM :: (forall x. m1 x -> m2 x) -> bm m1 a b -> bm m2 a b

toBiMapMaybe :: (IsBiMap bm, MonadInner m) => bm m edita editb -> bm Maybe edita editb
toBiMapMaybe = mapBiMapM mToMaybe

toBiMapResult ::
    forall bm m edita editb.
    (IsBiMap bm, MonadInner m) =>
    bm m edita editb ->
    bm (Result (Exc m)) edita editb
toBiMapResult = mapBiMapM retrieveInner

data Codec' m a b = MkCodec
    { decode :: a -> m b
    , encode :: b -> a
    }

-- must have decode . encode = Just

hoistCodec :: (forall x. m1 x -> m2 x) -> Codec' m1 a b -> Codec' m2 a b
hoistCodec f (MkCodec d e) = MkCodec (f . d) e

decodeMaybe :: MonadInner m => Codec' m a b -> a -> Maybe b
decodeMaybe codec = mToMaybe . decode codec

toCodec :: MonadInner m => Codec' m a b -> Codec a b
toCodec = hoistCodec mToMaybe

singleCodec :: (Alternative m, Eq a) => a -> Codec' m a ()
singleCodec val = let
    d a =
        if a == val
            then pure ()
            else empty
    e () = val
    in MkCodec d e

instance Functor m => Invariant (Codec' m p) where
    invmap ab ba (MkCodec d e) = MkCodec (\p -> fmap ab $ d p) (e . ba)

instance IsBiMap Codec' where
    mapBiMapM ff codec = MkCodec{decode = ff . (decode codec), encode = encode codec}

instance Alternative m => Summable (Codec' m p) where
    rVoid = MkCodec (\_ -> empty) absurd
    MkCodec d1 e1 <+++> MkCodec d2 e2 = let
        d12 p = (fmap Left $ d1 p) <|> (fmap Right $ d2 p)
        in MkCodec d12 (either e1 e2)

type Codec = Codec' Maybe

instance Monad m => Category (Codec' m) where
    id = MkCodec return id
    (MkCodec bmc cb) . (MkCodec amb ba) = MkCodec (\a -> (amb a) >>= bmc) (ba . cb)

ifCodec :: (a -> Bool) -> Codec a a
ifCodec f =
    MkCodec
        ( \a ->
            if f a
                then Just a
                else Nothing
        )
        id

justCodec :: Codec (Maybe a) a
justCodec = MkCodec id Just

bijectionCodec :: Applicative m => Bijection a b -> Codec' m a b
bijectionCodec (MkIsomorphism p q) = MkCodec (pure . p) q

codecBijection :: Monad m => Codec' m a b -> Bijection (m a) (m b)
codecBijection (MkCodec amb ba) = MkIsomorphism (\ma -> ma >>= amb) (fmap ba)

codecSum :: Summable f => Codec a b -> f b -> f a -> f a
codecSum MkCodec{..} fb fa =
    invmap
        (either encode id)
        ( \a ->
            case decode a of
                Just b -> Left b
                Nothing -> Right a
        )
        $ fb
        <+++> fa

instance (Traversable f, Applicative m) => CatFunctor (Codec' m) (Codec' m) f where
    cfmap codec = MkCodec{decode = traverse (decode codec), encode = fmap (encode codec)}

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

codecMap' :: (CodecMap f, MonadInner m) => Codec' m a b -> f a -> f b
codecMap' codec = codecMap $ toCodec codec

utf8Codec :: Codec' (Result UnicodeException) StrictByteString Text
utf8Codec = MkCodec (eitherToResult . decodeUtf8') encodeUtf8
