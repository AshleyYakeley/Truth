module Pinafore.Base.Literal
    ( Literal(..)
    , AsLiteral(..)
    , literalToMedia
    , mediaToLiteral
    , pattern MkMediaLiteral
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
    ]

addSerializer :: (MediaType, [Word8]) -> Serializer MediaType -> Serializer MediaType
addSerializer (t, bcode) s = let
    fromE :: Either () MediaType -> MediaType
    fromE (Left ()) = t
    fromE (Right t') = t'
    toE :: MediaType -> Either () MediaType
    toE t'
        | t' == t = Left ()
    toE t' = Right t'
    in invmap fromE toE (rLiteralBytes bcode <+++> s)

rawMediaTypeSerializer :: Serializer MediaType
rawMediaTypeSerializer = rLiteralBytes [0x6D] ***> serializer

headerSerializer :: Serializer MediaType
headerSerializer = foldr addSerializer rawMediaTypeSerializer mediaTypeCompress

givenMediaTypeSerializer :: MediaType -> Serializer ()
givenMediaTypeSerializer t =
    case lookup t mediaTypeCompress of
        Just bcode -> rLiteralBytes bcode
        Nothing -> rExact t rawMediaTypeSerializer

mediaToLiteralRaw :: MediaType -> StrictByteString -> Literal
mediaToLiteralRaw t b = MkLiteral $ serializerStrictEncode (headerSerializer <***> rWhole) (t, b)

mediaToLiteral :: Media -> Literal
mediaToLiteral (MkMedia t b) =
    case t of
        MkMediaType TextMediaType "plain" [] -> mediaToLiteralRaw plainTextMediaType b
        _ -> mediaToLiteralRaw t b

literalToMedia :: Literal -> Maybe Media
literalToMedia (MkLiteral bs) = do
    (t, b) <- serializerStrictDecode (headerSerializer <***> rWhole) bs
    return $ MkMedia t b

pattern MkMediaLiteral :: Media -> Literal

pattern MkMediaLiteral m <- (literalToMedia -> Just m)
  where MkMediaLiteral m = mediaToLiteral m

class Eq t => AsLiteral t where
    literalCodec :: Codec Literal t
    default literalCodec :: AsMediaLiteral t => Codec Literal t
    literalCodec = serializerStrictCodec literalSerializer . bijectionCodec coerceIsomorphism

toLiteral :: AsLiteral t => t -> Literal
toLiteral = encode literalCodec

fromLiteral :: AsLiteral t => Literal -> Maybe t
fromLiteral = decode literalCodec

literalToEntity :: AsLiteral t => t -> Entity
literalToEntity v = MkEntity $ byteStringToAnchor $ unLiteral $ toLiteral v

class AsLiteral t => AsMediaLiteral t where
    literalMediaType :: MediaType
    literalContentSerializer :: Serializer t

literalSerializer ::
       forall t. AsMediaLiteral t
    => Serializer t
literalSerializer = givenMediaTypeSerializer (literalMediaType @t) ***> literalContentSerializer

entityToLiteral :: Entity -> Maybe Literal
entityToLiteral (MkEntity anchor) = do
    bs <- anchorToByteString anchor
    return $ MkLiteral bs

instance AsLiteral Literal where
    literalCodec = id

instance AsLiteral Void where
    literalCodec = rVoid

instance AsLiteral StrictByteString

instance AsMediaLiteral StrictByteString where
    literalMediaType = octetStreamMediaType
    literalContentSerializer = serializer

instance AsLiteral Text

instance AsMediaLiteral Text where
    literalMediaType = plainTextMediaType
    literalContentSerializer = codecMap' utf8Codec rWhole

instance AsLiteral String where
    literalCodec = bijectionCodec unpackBijection . literalCodec @Text

instance AsLiteral ()

instance AsMediaLiteral () where
    literalMediaType = vndMediaType "unit"
    literalContentSerializer = rUnit

instance AsLiteral Bool

instance AsMediaLiteral Bool where
    literalMediaType = vndMediaType "boolean"
    literalContentSerializer = serializer

instance AsLiteral Ordering

instance AsMediaLiteral Ordering where
    literalMediaType = vndMediaType "ordering"
    literalContentSerializer = codecMap readShowCodec serializer

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
    literalContentSerializer = serializer

instance AsLiteral Double

instance AsMediaLiteral Double where
    literalMediaType = vndMediaType "double"
    literalContentSerializer = serializer

instance AsLiteral SafeRational where
    literalCodec = codecMap safeRationalNumber literalCodec

instance AsLiteral Integer where
    literalCodec = codecMap integerSafeRational literalCodec

instance AsLiteral Day

instance AsMediaLiteral Day where
    literalMediaType = vndMediaType "day"
    literalContentSerializer = serializer

instance AsLiteral TimeOfDay

instance AsMediaLiteral TimeOfDay where
    literalMediaType = vndMediaType "timeofday"
    literalContentSerializer = serializer

instance AsLiteral LocalTime

instance AsMediaLiteral LocalTime where
    literalMediaType = vndMediaType "localtime"
    literalContentSerializer = serializer

instance AsLiteral UTCTime

instance AsMediaLiteral UTCTime where
    literalMediaType = vndMediaType "time"
    literalContentSerializer = serializer

instance AsLiteral NominalDiffTime

instance AsMediaLiteral NominalDiffTime where
    literalMediaType = vndMediaType "duration"
    literalContentSerializer = serializer
