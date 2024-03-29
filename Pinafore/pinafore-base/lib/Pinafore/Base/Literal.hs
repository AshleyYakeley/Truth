module Pinafore.Base.Literal
    ( Literal(..)
    , AsLiteral(..)
    , literalToMIME
    , mimeToLiteral
    , pattern MkMIMELiteral
    , toLiteral
    , fromLiteral
    , literalToEntity
    , entityToLiteral
    , AsMIMELiteral(..)
    ) where

import Changes.World.MIME
import Data.Time
import Pinafore.Base.Anchor
import Pinafore.Base.Entity
import Pinafore.Base.MIME
import Pinafore.Base.Number
import Pinafore.Base.SafeRational
import Shapes
import Shapes.Numeric

newtype Literal = MkLiteral
    { unLiteral :: StrictByteString
    } deriving newtype (Eq, HasSerializer)

instance Show Literal where
    show (MkLiteral t) = show t

mimeCompress :: [(MIMEContentType, [Word8])]
mimeCompress =
    [ (vndMIMEType "unit", [0x75]) -- [u]
    , (vndMIMEType "boolean", [0x62]) -- [b]
    , (plainTextMIMEType, [0x74]) -- [t]
    , (vndMIMEType "ordering", [0x6F]) -- [o]
    , (vndMIMEType "rational", [0x72]) -- [r]
    , (vndMIMEType "double", [0x64]) -- [d]
    , (vndMIMEType "day", [0x54, 0x64]) -- [Td]
    , (vndMIMEType "timeofday", [0x54, 0x6F]) -- [To]
    , (vndMIMEType "localtime", [0x54, 0x6C]) -- [Tl]
    , (vndMIMEType "time", [0x54, 0x75]) -- [Tu]
    , (vndMIMEType "duration", [0x54, 0x6E]) -- [Tn]
    , (vndMIMEType "colour", [0x63]) -- [c]
    ]

addSerializer :: (MIMEContentType, [Word8]) -> Serializer MIMEContentType -> Serializer MIMEContentType
addSerializer (t, bcode) s = let
    fromE :: Either () MIMEContentType -> MIMEContentType
    fromE (Left ()) = t
    fromE (Right t') = t'
    toE :: MIMEContentType -> Either () MIMEContentType
    toE t'
        | t' == t = Left ()
    toE t' = Right t'
    in invmap fromE toE (rLiteralBytes bcode <+++> s)

rawMIMETypeSerializer :: Serializer MIMEContentType
rawMIMETypeSerializer = rLiteralBytes [0x6D] ***> serializer

headerSerializer :: Serializer MIMEContentType
headerSerializer = foldr addSerializer rawMIMETypeSerializer mimeCompress

givenMIMETypeSerializer :: MIMEContentType -> Serializer ()
givenMIMETypeSerializer t =
    case lookup t mimeCompress of
        Just bcode -> rLiteralBytes bcode
        Nothing -> rExact t rawMIMETypeSerializer

mimeToLiteralRaw :: MIMEContentType -> StrictByteString -> Literal
mimeToLiteralRaw t b = MkLiteral $ serializerStrictEncode (headerSerializer <***> rWhole) (t, b)

mimeToLiteral :: MIME -> Literal
mimeToLiteral (MkMIME t b) =
    case t of
        MkMIMEContentType TextMimeType "plain" [] -> mimeToLiteralRaw plainTextMIMEType b
        _ -> mimeToLiteralRaw t b

literalToMIME :: Literal -> Maybe MIME
literalToMIME (MkLiteral bs) = do
    (t, b) <- serializerStrictDecode (headerSerializer <***> rWhole) bs
    return $ MkMIME t b

pattern MkMIMELiteral :: MIME -> Literal

pattern MkMIMELiteral m <- (literalToMIME -> Just m)
  where MkMIMELiteral m = mimeToLiteral m

class Eq t => AsLiteral t where
    literalCodec :: Codec Literal t
    default literalCodec :: AsMIMELiteral t => Codec Literal t
    literalCodec = serializerStrictCodec literalSerializer . bijectionCodec coerceIsomorphism

toLiteral :: AsLiteral t => t -> Literal
toLiteral = encode literalCodec

fromLiteral :: AsLiteral t => Literal -> Maybe t
fromLiteral = decode literalCodec

literalToEntity :: AsLiteral t => t -> Entity
literalToEntity v = MkEntity $ byteStringToAnchor $ unLiteral $ toLiteral v

class AsLiteral t => AsMIMELiteral t where
    literalMimeType :: MIMEContentType
    literalContentSerializer :: Serializer t

literalSerializer ::
       forall t. AsMIMELiteral t
    => Serializer t
literalSerializer = givenMIMETypeSerializer (literalMimeType @t) ***> literalContentSerializer

entityToLiteral :: Entity -> Maybe Literal
entityToLiteral (MkEntity anchor) = do
    bs <- anchorToByteString anchor
    return $ MkLiteral bs

instance AsLiteral Literal where
    literalCodec = id

instance AsLiteral Void where
    literalCodec = rVoid

instance AsLiteral Text

instance AsMIMELiteral Text where
    literalMimeType = plainTextMIMEType
    literalContentSerializer = codecMap' utf8Codec rWhole

instance AsLiteral String where
    literalCodec = bijectionCodec unpackBijection . literalCodec @Text

instance AsLiteral ()

instance AsMIMELiteral () where
    literalMimeType = vndMIMEType "unit"
    literalContentSerializer = rUnit

instance AsLiteral Bool

instance AsMIMELiteral Bool where
    literalMimeType = vndMIMEType "boolean"
    literalContentSerializer = serializer

instance AsLiteral Ordering

instance AsMIMELiteral Ordering where
    literalMimeType = vndMIMEType "ordering"
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

instance AsMIMELiteral Rational where
    literalMimeType = vndMIMEType "rational"
    literalContentSerializer = serializer

instance AsLiteral Double

instance AsMIMELiteral Double where
    literalMimeType = vndMIMEType "double"
    literalContentSerializer = serializer

instance AsLiteral SafeRational where
    literalCodec = codecMap safeRationalNumber literalCodec

instance AsLiteral Integer where
    literalCodec = codecMap integerSafeRational literalCodec

instance AsLiteral Day

instance AsMIMELiteral Day where
    literalMimeType = vndMIMEType "day"
    literalContentSerializer = serializer

instance AsLiteral TimeOfDay

instance AsMIMELiteral TimeOfDay where
    literalMimeType = vndMIMEType "timeofday"
    literalContentSerializer = serializer

instance AsLiteral LocalTime

instance AsMIMELiteral LocalTime where
    literalMimeType = vndMIMEType "localtime"
    literalContentSerializer = serializer

instance AsLiteral UTCTime

instance AsMIMELiteral UTCTime where
    literalMimeType = vndMIMEType "time"
    literalContentSerializer = serializer

instance AsLiteral NominalDiffTime

instance AsMIMELiteral NominalDiffTime where
    literalMimeType = vndMIMEType "duration"
    literalContentSerializer = serializer
