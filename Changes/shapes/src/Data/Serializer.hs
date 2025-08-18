module Data.Serializer where

import Codec.LEB128
import Codec.LEB128.Constraints
import Data.ByteString.Builder
import Data.PrimitiveSerial

import Data.Codec
import Data.Filterable
import Shapes.Import

data Stopping
    = Stops
    | KeepsGoing

data Serializer (stp :: Stopping) a = MkSerializer
    { serialize :: a -> Builder
    , deserialize :: BSRead a
    }

instance Invariant (Serializer stp) where
    invmap ab ba (MkSerializer s d) = MkSerializer (s . ba) (fmap ab d)

sUpstop :: Serializer 'Stops a -> Serializer stp a
sUpstop (MkSerializer s d) = MkSerializer s d

sleb128Serializer ::
    forall stp a.
    SLEB128 a =>
    Serializer stp a
sleb128Serializer =
    MkSerializer toSLEB128Builder
        $ MkBSRead
        $ \bs -> let
            (ma, bs') = fromSLEB128ByteString bs
            in fmap (\a -> (bs', a)) ma

sUnit :: Serializer stp ()
sUnit = let
    serialize () = mempty
    deserialize = return ()
    in MkSerializer{..}

sProduct :: forall stp a b. Serializer 'Stops a -> Serializer stp b -> Serializer stp (a, b)
sProduct (MkSerializer sa da) (MkSerializer sb db) = let
    sab (a, b) = sa a <> sb b
    dab = liftA2 (,) da db
    in MkSerializer sab dab

sProductR :: forall stp b. Serializer 'Stops () -> Serializer stp b -> Serializer stp b
sProductR (MkSerializer sa da) (MkSerializer sb db) = let
    sab b = sa () <> sb b
    dab = liftA2 (\() b -> b) da db
    in MkSerializer sab dab

instance Productable (Serializer 'Stops) where
    rUnit = sUnit
    (<***>) = sProduct

sVoid :: Serializer stp Void
sVoid = let
    serialize n = never n
    deserialize = empty
    in MkSerializer{..}

-- | only suitable if the two serializers are disjoint
sPick :: forall stp a b. Serializer stp a -> Serializer stp b -> Serializer stp (Either a b)
sPick (MkSerializer sa da) (MkSerializer sb db) = let
    sab (Left a) = sa a
    sab (Right b) = sb b
    dab = fmap Left da <|> fmap Right db
    in MkSerializer sab dab

instance Summable (Serializer stp) where
    rVoid = sVoid
    (<+++>) = sPick

sOptional :: Serializer 'Stops a -> Serializer 'KeepsGoing (Maybe a)
sOptional (MkSerializer s d) = let
    s' Nothing = mempty
    s' (Just x) = s x
    d' = fmap Just d <|> return Nothing
    in MkSerializer s' d'

sList :: Serializer 'Stops a -> Serializer 'KeepsGoing [a]
sList (MkSerializer s d) = let
    s' [] = mempty
    s' (x : xs) = s x <> s' xs
    d' = liftA2 (:) d d' <|> return []
    in MkSerializer s' d'

sCountedList :: forall stp a. Serializer 'Stops a -> Serializer stp [a]
sCountedList (MkSerializer s d) = let
    lengthSerializer :: Serializer 'Stops Int
    lengthSerializer = sleb128Serializer
    s' :: [a] -> Builder
    s' bs = serialize lengthSerializer (olength bs) <> concatmap s bs
    d' = do
        len <- deserialize lengthSerializer
        forn len d
    in MkSerializer s' d'

sList1 :: Serializer 'Stops a -> Serializer 'KeepsGoing (NonEmpty a)
sList1 (MkSerializer s d) = let
    s' [] = mempty
    s' (x : xs) = s'' (x :| xs)
    s'' (x :| xs) = s x <> s' xs
    d' = fmap (\(x :| xs) -> x : xs) d'' <|> return []
    d'' = liftA2 (:|) d d'
    in MkSerializer s'' d''

sItem :: Serializer stp Word8
sItem = MkSerializer word8 bsRead

sWhole :: Serializer 'KeepsGoing StrictByteString
sWhole = let
    serialize = byteString
    deserialize = bsReadEverything
    in MkSerializer{..}

sLiterals :: StrictByteString -> Serializer stp ()
sLiterals bs = let
    serialize () = byteString bs
    deserialize = do
        bs' <- bsReadN $ olength bs
        if bs' == bs
            then return ()
            else empty
    in MkSerializer{..}

sLiteral :: Word8 -> Serializer stp ()
sLiteral w = let
    serialize () = word8 w
    deserialize = do
        w' <- bsRead
        if w == w'
            then return ()
            else empty
    in MkSerializer{..}

sExact ::
    forall stp a.
    Eq a =>
    a ->
    Serializer stp a ->
    Serializer stp ()
sExact a (MkSerializer s d) = let
    serialize () = s a
    deserialize = do
        a' <- d
        if a == a'
            then return ()
            else empty
    in MkSerializer{..}

instance InjectiveFilterable (Serializer stp) where
    injectiveFilter MkCodec{..} (MkSerializer s d) =
        MkSerializer (s . encode) $ do
            a <- d
            mpure $ decode a

littleEndianSerializer ::
    forall stp a.
    FixedNumeric a =>
    Serializer stp a
littleEndianSerializer = MkSerializer (byteString . encodeLittleEndian) decodeLittleEndian

sLiteralBytes :: [Word8] -> Serializer stp ()
sLiteralBytes ws = sLiterals $ pack ws

fixedByteStringSerializer :: Int -> Serializer stp StrictByteString
fixedByteStringSerializer i = let
    serialize = byteString
    deserialize = bsReadN i
    in MkSerializer{..}

-- | note length is bytes, not chars
fixedTextSerializer :: Int -> Serializer stp Text
fixedTextSerializer i = injectiveFilter' utf8Codec $ fixedByteStringSerializer i

serializerLazyCodec :: forall stp a. Serializer stp a -> Codec LazyByteString a
serializerLazyCodec (MkSerializer s d) = let
    encode a = toLazyByteString $ s a
    decode lbs = runWholeBSRead d $ toStrict lbs
    in MkCodec{..}

serializerStrictEncode :: forall stp a. Serializer stp a -> a -> StrictByteString
serializerStrictEncode (MkSerializer s _) a = toStrict $ toLazyByteString $ s a

serializerStrictDecode :: forall stp a. Serializer stp a -> StrictByteString -> Maybe a
serializerStrictDecode (MkSerializer _ d) = runWholeBSRead d

serializerStrictCodec :: forall stp a. Serializer stp a -> Codec StrictByteString a
serializerStrictCodec sr = let
    encode = serializerStrictEncode sr
    decode = serializerStrictDecode sr
    in MkCodec{..}

codecSerializer :: Codec StrictByteString a -> Serializer 'KeepsGoing a
codecSerializer codec = injectiveFilter codec sWhole
