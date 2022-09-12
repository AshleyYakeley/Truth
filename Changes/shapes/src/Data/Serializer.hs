module Data.Serializer where

import Data.Codec
import qualified Data.Serialize as Serialize
import Data.Streamable
import Shapes.Import

data Serializer a = MkSerializer
    { serialize :: Serialize.Putter a
    , deserialize :: Serialize.Get a
    }

instance Invariant Serializer where
    invmap ab ba (MkSerializer s d) = MkSerializer (s . ba) (fmap ab d)

instance Productable Serializer where
    rUnit :: Serializer ()
    rUnit = let
        serialize () = return ()
        deserialize = return ()
        in MkSerializer {..}
    (<***>) :: forall a b. Serializer a -> Serializer b -> Serializer (a, b)
    MkSerializer sa da <***> MkSerializer sb db = let
        sab (a, b) = sa a >> sb b
        dab = liftA2 (,) da db
        in MkSerializer sab dab

instance Summable Serializer where
    rVoid :: Serializer Void
    rVoid = let
        serialize n = never n
        deserialize = empty
        in MkSerializer {..}
    MkSerializer sa da <+++> MkSerializer sb db = let
        sab (Left a) = sa a
        sab (Right b) = sb b
        dab = fmap Left da <|> fmap Right db
        in MkSerializer sab dab

instance Riggable Serializer where
    rOptional (MkSerializer s d) = let
        s' Nothing = mempty
        s' (Just x) = s x
        d' = fmap Just d <|> return Nothing
        in MkSerializer s' d'
    rList (MkSerializer s d) = let
        s' [] = mempty
        s' (x:xs) = s x <> s' xs
        d' = liftA2 (:) d d' <|> return []
        in MkSerializer s' d'
    rList1 (MkSerializer s d) = let
        s' [] = mempty
        s' (x:xs) = s'' (x :| xs)
        s'' (x :| xs) = s x <> s' xs
        d' = fmap (\(x :| xs) -> x : xs) d'' <|> return []
        d'' = liftA2 (:|) d d'
        in MkSerializer s'' d''

instance Streamable Serializer where
    type StreamableBasis Serializer = StrictByteString
    rItem = MkSerializer Serialize.putWord8 Serialize.getWord8
    rWhole = let
        serialize = Serialize.putByteString
        deserialize = do
            n <- Serialize.remaining
            Serialize.getByteString n
        in MkSerializer {..}
    rLiterals bs = let
        serialize () = Serialize.putByteString bs
        deserialize = do
            bs' <- Serialize.getByteString $ olength bs
            if bs' == bs
                then return ()
                else empty
        in MkSerializer {..}
    rLiteral w = let
        serialize () = Serialize.putWord8 w
        deserialize = do
            w' <- Serialize.getWord8
            if w == w'
                then return ()
                else empty
        in MkSerializer {..}
    rExact a (MkSerializer s d) = let
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
pLiteralBytes ws = rLiterals $ pack ws

serializerLazyCodec ::
       forall m a. MonadFail m
    => Serializer a
    -> Codec' m LazyByteString a
serializerLazyCodec (MkSerializer s d) = let
    encode a = Serialize.runPutLazy $ s a
    decode = resultToM . eitherToResult . Serialize.runGetLazy d
    in MkCodec {..}

serializerStrictEncode :: Serializer a -> a -> StrictByteString
serializerStrictEncode (MkSerializer s _) a = Serialize.runPut $ s a

serializerStrictDecode :: MonadFail m => Serializer a -> StrictByteString -> m a
serializerStrictDecode (MkSerializer _ d) bs = resultToM $ eitherToResult $ Serialize.runGet d bs

serializerStrictCodec ::
       forall m a. MonadFail m
    => Serializer a
    -> Codec' m StrictByteString a
serializerStrictCodec sr = let
    encode = serializerStrictEncode sr
    decode = serializerStrictDecode sr
    in MkCodec {..}

serializeLazyCodec ::
       forall m t. (MonadFail m, Serialize t)
    => Codec' m LazyByteString t
serializeLazyCodec = serializerLazyCodec serializer

serializeStrictCodec ::
       forall m t. (MonadFail m, Serialize t)
    => Codec' m StrictByteString t
serializeStrictCodec = serializerStrictCodec serializer
