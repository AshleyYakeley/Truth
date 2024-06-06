module Pinafore.Base.Literal
    ( Literal(..)
    , AsLiteral(..)
    , literalMediaCodec
    , toLiteral
    , fromLiteral
    , literalToEntity
    , entityToLiteral
    , AsMediaLiteral(..)
    ) where

import Changes.World.Media.Type
import Data.Time
import Pinafore.Base.Anchor
import Pinafore.Base.Entity
import Pinafore.Base.Media
import Pinafore.Base.Number
import Pinafore.Base.SafeRational
import Shapes
import Shapes.Numeric

newtype Literal = MkLiteral
    { unLiteral :: StrictByteString
    } deriving newtype (Eq, HasSerializer)

instance Show Literal where
    show (MkLiteral t) = show t

mediaTypeCompress :: [(MediaType, [Word8])]
mediaTypeCompress =
    [ (vndMediaType "unit", [0x75]) -- [u]
    , (vndMediaType "boolean", [0x62]) -- [b]
    , (octetStreamMediaType, [0x61]) -- [a]
    , (plainTextMediaType, [0x74]) -- [t]
    , (vndMediaType "ordering", [0x6F]) -- [o]
    , (vndMediaType "rational", [0x72]) -- [r]
    , (vndMediaType "double", [0x64]) -- [d]
    , (vndMediaType "day", [0x54, 0x64]) -- [Td]
    , (vndMediaType "timeofday", [0x54, 0x6F]) -- [To]
    , (vndMediaType "localtime", [0x54, 0x6C]) -- [Tl]
    , (vndMediaType "time", [0x54, 0x75]) -- [Tu]
    , (vndMediaType "duration", [0x54, 0x6E]) -- [Tn]
    , (vndMediaType "colour", [0x63]) -- [c]
    , (vndMediaType "media-type", [0x79]) -- [y]
    , (vndMediaType "media", [0x6D]) -- [m]
    ]

addSerializer :: (MediaType, [Word8]) -> Serializer Stops MediaType -> Serializer Stops MediaType
addSerializer (t, bcode) s = let
    fromE :: Either () MediaType -> MediaType
    fromE (Left ()) = t
    fromE (Right t') = t'
    toE :: MediaType -> Either () MediaType
    toE t'
        | t' == t = Left ()
    toE t' = Right t'
    in invmap fromE toE (sPick (sLiteralBytes bcode) s)

rawMediaTypeSerializer :: Serializer Stops MediaType
rawMediaTypeSerializer = sLiteralBytes [0x6D] ***> stoppingSerializer

mediaTypeSerializer :: Serializer Stops MediaType
mediaTypeSerializer = foldr addSerializer rawMediaTypeSerializer mediaTypeCompress

givenMediaTypeSerializer :: MediaType -> Serializer Stops ()
givenMediaTypeSerializer t =
    case lookup t mediaTypeCompress of
        Just bcode -> sLiteralBytes bcode
        Nothing -> sExact t rawMediaTypeSerializer

literalMediaCodec :: Codec Literal Media
literalMediaCodec = let
    encodeRaw :: MediaType -> StrictByteString -> Literal
    encodeRaw t b = MkLiteral $ serializerStrictEncode (sProduct mediaTypeSerializer sWhole) (t, b)
    encode :: Media -> Literal
    encode (MkMedia t b) =
        case t of
            MkMediaType TextMediaType "plain" [] -> encodeRaw plainTextMediaType b
            _ -> encodeRaw t b
    decode :: Literal -> Maybe Media
    decode (MkLiteral bs) = do
        (t, b) <- serializerStrictDecode (sProduct mediaTypeSerializer sWhole) bs
        return $ MkMedia t b
    in MkCodec {..}

class Eq t => AsLiteral t where
    literalCodec :: Codec Literal t
    default literalCodec :: AsMediaLiteral t => Codec Literal t
    literalCodec = serializerStrictCodec literalSerializer . coerceCodec

toLiteral :: AsLiteral t => t -> Literal
toLiteral = encode literalCodec

fromLiteral :: AsLiteral t => Literal -> Maybe t
fromLiteral = decode literalCodec

literalToEntity :: AsLiteral t => t -> Entity
literalToEntity v = MkEntity $ byteStringToAnchor $ unLiteral $ toLiteral v

class AsLiteral t => AsMediaLiteral t where
    literalMediaType :: MediaType
    literalContentSerializer :: Serializer KeepsGoing t
    default literalContentSerializer :: HasSerializer t => Serializer KeepsGoing t
    literalContentSerializer = greedySerializer

literalSerializer ::
       forall t. AsMediaLiteral t
    => Serializer KeepsGoing t
literalSerializer = sProductR (givenMediaTypeSerializer (literalMediaType @t)) literalContentSerializer

entityToLiteral :: Entity -> Maybe Literal
entityToLiteral (MkEntity anchor) = do
    bs <- anchorToByteString anchor
    return $ MkLiteral bs

instance AsLiteral Literal where
    literalCodec = id

instance AsLiteral MediaType

instance AsMediaLiteral MediaType where
    literalMediaType = vndMediaType "media-type" -- don't compress

instance AsLiteral Media

instance AsMediaLiteral Media where
    literalMediaType = vndMediaType "media"

instance AsLiteral Void where
    literalCodec = rVoid

instance AsLiteral StrictByteString

instance AsMediaLiteral StrictByteString where
    literalMediaType = octetStreamMediaType

instance AsLiteral Text

instance AsMediaLiteral Text where
    literalMediaType = plainTextMediaType
    literalContentSerializer = codecMap' utf8Codec sWhole

instance AsLiteral String where
    literalCodec = bijectionCodec unpackBijection . literalCodec @Text

instance AsLiteral ()

instance AsMediaLiteral () where
    literalMediaType = vndMediaType "unit"

instance AsLiteral Bool

instance AsMediaLiteral Bool where
    literalMediaType = vndMediaType "boolean"

instance AsLiteral Ordering

instance AsMediaLiteral Ordering where
    literalMediaType = vndMediaType "ordering"

instance AsLiteral Number where
    literalCodec = let
        eitherToNumber :: Either Rational Double -> Number
        eitherToNumber (Left x) = ExactNumber x
        eitherToNumber (Right x) = InexactNumber x
        numberToEither :: Number -> Either Rational Double
        numberToEither (ExactNumber x) = Left x
        numberToEither (InexactNumber x) = Right x
        in invmap eitherToNumber numberToEither $ literalCodec <+++> literalCodec

instance AsLiteral Rational

instance AsMediaLiteral Rational where
    literalMediaType = vndMediaType "rational"

instance AsLiteral Double

instance AsMediaLiteral Double where
    literalMediaType = vndMediaType "double"

instance AsLiteral SafeRational where
    literalCodec = codecMap safeRationalNumber literalCodec

instance AsLiteral Integer where
    literalCodec = codecMap integerSafeRational literalCodec

instance AsLiteral Day

instance AsMediaLiteral Day where
    literalMediaType = vndMediaType "day"

instance AsLiteral TimeOfDay

instance AsMediaLiteral TimeOfDay where
    literalMediaType = vndMediaType "timeofday"

instance AsLiteral LocalTime

instance AsMediaLiteral LocalTime where
    literalMediaType = vndMediaType "localtime"

instance AsLiteral UTCTime

instance AsMediaLiteral UTCTime where
    literalMediaType = vndMediaType "time"

instance AsLiteral NominalDiffTime

instance AsMediaLiteral NominalDiffTime where
    literalMediaType = vndMediaType "duration"
