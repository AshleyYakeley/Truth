module Data.Serializer
    ( Serializer
    , rLiteralBytes
    , fixedByteStringSerializer
    , serializerLazyCodec
    , serializerStrictEncode
    , serializerStrictDecode
    , serializerStrictCodec
    , HasSerializer(..)
    , serializeLazyCodec
    , serializeStrictCodec
    ) where

import Codec.LEB128
import Codec.LEB128.Constraints
import Data.ByteString.Builder
import Data.Codec
import Data.PrimitiveSerial
import Data.Streamable
import Shapes.Import
import Shapes.Numeric

data Serializer a = MkSerializer
    { serialize :: a -> Builder
    , deserialize :: BSRead a
    }

instance Invariant Serializer where
    invmap ab ba (MkSerializer s d) = MkSerializer (s . ba) (fmap ab d)

instance Productable Serializer where
    rUnit :: Serializer ()
    rUnit = let
        serialize () = mempty
        deserialize = return ()
        in MkSerializer {..}
    (<***>) :: forall a b. Serializer a -> Serializer b -> Serializer (a, b)
    MkSerializer sa da <***> MkSerializer sb db = let
        sab (a, b) = sa a <> sb b
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
    rItem = MkSerializer word8 bsRead
    rWhole = let
        serialize = byteString
        deserialize = bsReadEverything
        in MkSerializer {..}
    rLiterals bs = let
        serialize () = byteString bs
        deserialize = do
            bs' <- bsReadN $ olength bs
            if bs' == bs
                then return ()
                else empty
        in MkSerializer {..}
    rLiteral w = let
        serialize () = word8 w
        deserialize = do
            w' <- bsRead
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

littleEndianSerializer ::
       forall a. FixedNumeric a
    => Serializer a
littleEndianSerializer = MkSerializer (byteString . encodeLittleEndian) decodeLittleEndian

rLiteralBytes :: [Word8] -> Serializer ()
rLiteralBytes ws = rLiterals $ pack ws

fixedByteStringSerializer :: Int -> Serializer StrictByteString
fixedByteStringSerializer i = let
    serialize = byteString
    deserialize = bsReadN i
    in MkSerializer {..}

serializerLazyCodec :: forall a. Serializer a -> Codec LazyByteString a
serializerLazyCodec (MkSerializer s d) = let
    encode a = toLazyByteString $ s a
    decode lbs = runWholeBSRead d $ toStrict lbs
    in MkCodec {..}

serializerStrictEncode :: Serializer a -> a -> StrictByteString
serializerStrictEncode (MkSerializer s _) a = toStrict $ toLazyByteString $ s a

serializerStrictDecode :: Serializer a -> StrictByteString -> Maybe a
serializerStrictDecode (MkSerializer _ d) = runWholeBSRead d

serializerStrictCodec :: forall a. Serializer a -> Codec StrictByteString a
serializerStrictCodec sr = let
    encode = serializerStrictEncode sr
    decode = serializerStrictDecode sr
    in MkCodec {..}

class HasSerializer a where
    serializer :: Serializer a

serializeLazyCodec ::
       forall t. HasSerializer t
    => Codec LazyByteString t
serializeLazyCodec = serializerLazyCodec serializer

serializeStrictCodec ::
       forall t. HasSerializer t
    => Codec StrictByteString t
serializeStrictCodec = serializerStrictCodec serializer

instance HasSerializer Bool where
    serializer = let
        encode False = 0
        encode True = 1
        decode 0 = Just False
        decode 1 = Just True
        decode _ = Nothing
        in codecMap MkCodec {..} rItem

instance HasSerializer Word8 where
    serializer = littleEndianSerializer

instance HasSerializer Word16 where
    serializer = littleEndianSerializer

instance HasSerializer Word32 where
    serializer = littleEndianSerializer

instance HasSerializer Word64 where
    serializer = littleEndianSerializer

instance HasSerializer Int8 where
    serializer = littleEndianSerializer

instance HasSerializer Int16 where
    serializer = littleEndianSerializer

instance HasSerializer Int32 where
    serializer = littleEndianSerializer

instance HasSerializer Int64 where
    serializer = littleEndianSerializer

instance HasSerializer Float where
    serializer = littleEndianSerializer

instance HasSerializer Double where
    serializer = littleEndianSerializer

sleb128Serializer ::
       forall a. SLEB128 a
    => Serializer a
sleb128Serializer =
    MkSerializer toSLEB128Builder $
    MkBSRead $ \bs -> let
        (ma, bs') = fromSLEB128ByteString bs
        in fmap (\a -> (bs', a)) ma

instance HasSerializer Integer where
    serializer = sleb128Serializer

instance HasSerializer Rational where
    serializer = let
        tupleToRational :: (Integer, Integer) -> Rational
        tupleToRational (n, d) = n % d
        rationalToTuple :: Rational -> (Integer, Integer)
        rationalToTuple r = (numerator r, denominator r)
        in invmap tupleToRational rationalToTuple $ serializer <***> serializer

instance HasSerializer StrictByteString where
    serializer = let
        lengthSerializer :: Serializer Int
        lengthSerializer = sleb128Serializer
        s :: StrictByteString -> Builder
        s bs = serialize lengthSerializer (olength bs) <> byteString bs
        d = do
            len <- deserialize lengthSerializer
            bsReadN len
        in MkSerializer s d
