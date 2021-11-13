module Data.Serializer where

import Control.Monad.Ology.Result
import Data.Codec
import Data.IsoVariant
import qualified Data.Serialize as Serialize
import Data.Streamish
import Shapes.Import

data Serializer a = MkSerializer
    { serialize :: Serialize.Putter a
    , deserialize :: Serialize.Get a
    }

instance IsoVariant Serializer where
    isoMap ab ba (MkSerializer s d) = MkSerializer (s . ba) (fmap ab d)

instance Productish Serializer where
    pUnit :: Serializer ()
    pUnit = let
        serialize () = return ()
        deserialize = return ()
        in MkSerializer {..}
    (<***>) :: forall a b. Serializer a -> Serializer b -> Serializer (a, b)
    MkSerializer sa da <***> MkSerializer sb db = let
        sab (a, b) = sa a >> sb b
        dab = liftA2 (,) da db
        in MkSerializer sab dab

instance Summish Serializer where
    pNone :: Serializer None
    pNone = let
        serialize n = never n
        deserialize = empty
        in MkSerializer {..}
    MkSerializer sa da <+++> MkSerializer sb db = let
        sab (Left a) = sa a
        sab (Right b) = sb b
        dab = fmap Left da <|> fmap Right db
        in MkSerializer sab dab

instance Ringish Serializer where
    pOptional (MkSerializer s d) = let
        s' Nothing = mempty
        s' (Just x) = s x
        d' = fmap Just d <|> return Nothing
        in MkSerializer s' d'
    pList (MkSerializer s d) = let
        s' [] = mempty
        s' (x:xs) = s x <> s' xs
        d' = liftA2 (:) d d' <|> return []
        in MkSerializer s' d'
    pList1 (MkSerializer s d) = let
        s' [] = mempty
        s' (x:xs) = s'' (x :| xs)
        s'' (x :| xs) = s x <> s' xs
        d' = fmap (\(x :| xs) -> x : xs) d'' <|> return []
        d'' = liftA2 (:|) d d'
        in MkSerializer s'' d''

instance Streamish Serializer where
    type StreamishBasis Serializer = StrictByteString
    pItem = MkSerializer Serialize.putWord8 Serialize.getWord8
    pLiterals bs = let
        serialize () = Serialize.putByteString bs
        deserialize = do
            bs' <- Serialize.getByteString $ olength bs
            if bs' == bs
                then return ()
                else empty
        in MkSerializer {..}
    pLiteral w = let
        serialize () = Serialize.putWord8 w
        deserialize = do
            w' <- Serialize.getWord8
            if w == w'
                then return ()
                else empty
        in MkSerializer {..}
    pExact a (MkSerializer s d) = let
        serialize () = s a
        deserialize = do
            a' <- d
            if a == a'
                then return ()
                else empty
        in MkSerializer {..}

instance CodecMap Serializer where
    codecMap MkCodec {..} (MkSerializer s d) =
        MkSerializer (s . encode) $ do
            a <- d
            mpure $ decode a

serializer :: Serialize a => Serializer a
serializer = MkSerializer Serialize.put Serialize.get

pLiteralBytes :: [Word8] -> Serializer ()
pLiteralBytes ws = pLiterals $ pack ws

serializerWhole :: Serializer StrictByteString
serializerWhole = let
    serialize = Serialize.putByteString
    deserialize = do
        n <- Serialize.remaining
        Serialize.getByteString n
    in MkSerializer {..}

serializerLazyCodec ::
       forall m a. MonadFail m
    => Serializer a
    -> Codec' m LazyByteString a
serializerLazyCodec (MkSerializer s d) = let
    encode a = Serialize.runPutLazy $ s a
    decode = resultToM . eitherToResult . Serialize.runGetLazy d
    in MkCodec {..}

serializerStrictCodec ::
       forall m a. MonadFail m
    => Serializer a
    -> Codec' m StrictByteString a
serializerStrictCodec (MkSerializer s d) = let
    encode a = Serialize.runPut $ s a
    decode = resultToM . eitherToResult . Serialize.runGet d
    in MkCodec {..}

serializeLazyCodec ::
       forall m t. (MonadFail m, Serialize t)
    => Codec' m LazyByteString t
serializeLazyCodec = serializerLazyCodec serializer

serializeStrictCodec ::
       forall m t. (MonadFail m, Serialize t)
    => Codec' m StrictByteString t
serializeStrictCodec = serializerStrictCodec serializer
