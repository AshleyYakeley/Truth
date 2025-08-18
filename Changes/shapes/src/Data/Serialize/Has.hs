module Data.Serialize.Has where

import Data.ByteString.Builder
import Data.PrimitiveSerial
import Data.Time

import Data.Codec
import Data.Coerce.Coercion
import Data.Filterable
import Data.Serializer
import Shapes.Import
import Shapes.Numeric

class HasSerializer a where
    stoppingSerializer :: forall stp. Serializer stp a
    greedySerializer :: Serializer 'KeepsGoing a
    greedySerializer = stoppingSerializer

serializeLazyCodec ::
    forall a.
    HasSerializer a =>
    Codec LazyByteString a
serializeLazyCodec = serializerLazyCodec stoppingSerializer

serializeStrictCodec ::
    forall a.
    HasSerializer a =>
    Codec StrictByteString a
serializeStrictCodec = serializerStrictCodec stoppingSerializer

instance HasSerializer Void where
    stoppingSerializer = sVoid

instance HasSerializer () where
    greedySerializer = sUnit
    stoppingSerializer = sLiteral 0 -- necessary for lists to work

instance (HasSerializer a, HasSerializer b) => HasSerializer (a, b) where
    greedySerializer = sProduct stoppingSerializer greedySerializer
    stoppingSerializer = sProduct stoppingSerializer stoppingSerializer

instance {-# OVERLAPPABLE #-} HasSerializer a => HasSerializer [a] where
    greedySerializer = sList stoppingSerializer
    stoppingSerializer = sCountedList stoppingSerializer

instance HasSerializer StrictByteString where
    greedySerializer = sWhole
    stoppingSerializer = let
        lengthSerializer :: Serializer 'Stops Int
        lengthSerializer = sleb128Serializer
        s :: StrictByteString -> Builder
        s bs = serialize lengthSerializer (olength bs) <> byteString bs
        d = do
            len <- deserialize lengthSerializer
            bsReadN len
        in MkSerializer s d

instance HasSerializer Bool where
    stoppingSerializer = let
        encode False = 0
        encode True = 1
        decode 0 = Just False
        decode 1 = Just True
        decode _ = Nothing
        in injectiveFilter MkCodec{..} sItem

instance HasSerializer Word8 where
    stoppingSerializer = littleEndianSerializer

instance HasSerializer Word16 where
    stoppingSerializer = littleEndianSerializer

instance HasSerializer Word32 where
    stoppingSerializer = littleEndianSerializer

instance HasSerializer Word64 where
    stoppingSerializer = littleEndianSerializer

instance HasSerializer Int8 where
    stoppingSerializer = littleEndianSerializer

instance HasSerializer Int16 where
    stoppingSerializer = littleEndianSerializer

instance HasSerializer Int32 where
    stoppingSerializer = littleEndianSerializer

instance HasSerializer Int64 where
    stoppingSerializer = littleEndianSerializer

instance HasSerializer Float where
    stoppingSerializer = littleEndianSerializer

instance HasSerializer Double where
    stoppingSerializer = littleEndianSerializer

instance HasSerializer Integer where
    stoppingSerializer = sleb128Serializer

instance HasSerializer Rational where
    stoppingSerializer = let
        tupleToRational :: (Integer, Integer) -> Rational
        tupleToRational (n, d) = n % d
        rationalToTuple :: Rational -> (Integer, Integer)
        rationalToTuple r = (numerator r, denominator r)
        in invmap tupleToRational rationalToTuple $ sProduct stoppingSerializer stoppingSerializer

instance HasSerializer Ordering where
    stoppingSerializer = injectiveFilter readShowCodec $ invmap unpack pack $ fixedTextSerializer 2

instance HasSerializer Text where
    greedySerializer = invmap decodeUtf8 encodeUtf8 $ greedySerializer @StrictByteString
    stoppingSerializer = invmap decodeUtf8 encodeUtf8 $ stoppingSerializer @StrictByteString

instance HasSerializer String where
    greedySerializer = invmap unpack pack $ greedySerializer @Text
    stoppingSerializer = invmap unpack pack $ stoppingSerializer @Text

instance HasSerializer (Fixed a) where
    stoppingSerializer = isoCoerce $ stoppingSerializer @Integer

instance HasSerializer DiffTime where
    stoppingSerializer = invmap picosecondsToDiffTime diffTimeToPicoseconds stoppingSerializer

instance HasSerializer NominalDiffTime where
    stoppingSerializer = invmap secondsToNominalDiffTime nominalDiffTimeToSeconds stoppingSerializer

instance HasSerializer Day where
    stoppingSerializer = isoCoerce $ stoppingSerializer @Integer

instance HasSerializer TimeOfDay where
    stoppingSerializer = let
        tupleToTimeOfDay :: (Word8, (Word8, Word64)) -> TimeOfDay
        tupleToTimeOfDay (h, (m, s)) = TimeOfDay (fromIntegral h) (fromIntegral m) (MkFixed $ toInteger s)
        timeOfDayToTuple :: TimeOfDay -> (Word8, (Word8, Word64))
        timeOfDayToTuple (TimeOfDay h m (MkFixed s)) = (fromIntegral h, (fromIntegral m, fromInteger s))
        in invmap tupleToTimeOfDay timeOfDayToTuple
            $ sProduct stoppingSerializer
            $ sProduct stoppingSerializer stoppingSerializer

instance HasSerializer LocalTime where
    stoppingSerializer = let
        pairToLocalTime :: (Day, TimeOfDay) -> LocalTime
        pairToLocalTime (d, t) = LocalTime d t
        localTimeToPair :: LocalTime -> (Day, TimeOfDay)
        localTimeToPair (LocalTime d t) = (d, t)
        in invmap pairToLocalTime localTimeToPair $ sProduct stoppingSerializer stoppingSerializer

instance HasSerializer UTCTime where
    stoppingSerializer = let
        pairToUTCTime :: (Day, DiffTime) -> UTCTime
        pairToUTCTime (d, t) = UTCTime d t
        utcTimeToPair :: UTCTime -> (Day, DiffTime)
        utcTimeToPair (UTCTime d t) = (d, t)
        in invmap pairToUTCTime utcTimeToPair $ sProduct stoppingSerializer stoppingSerializer
