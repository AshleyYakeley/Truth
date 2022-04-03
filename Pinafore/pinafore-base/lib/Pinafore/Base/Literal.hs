module Pinafore.Base.Literal where

import Data.Time
import Pinafore.Base.Anchor
import Pinafore.Base.Entity
import Pinafore.Base.Number
import Pinafore.Base.SafeRational
import Shapes
import Shapes.Numeric

newtype Literal = MkLiteral
    { unLiteral :: StrictByteString
    } deriving (Eq, Serialize)

instance Show Literal where
    show (MkLiteral t) = show t

class Eq t => AsLiteral t where
    literalSerializer :: Serializer t

literalCodec :: AsLiteral t => Codec Literal t
literalCodec = serializerStrictCodec literalSerializer . bijectionCodec coerceIsomorphism

toLiteral :: AsLiteral t => t -> Literal
toLiteral = encode literalCodec

fromLiteral :: AsLiteral t => Literal -> Maybe t
fromLiteral = decode literalCodec

literalToEntity :: AsLiteral t => t -> Entity
literalToEntity v = MkEntity $ byteStringToAnchor $ unLiteral $ toLiteral v

entityToLiteral :: Entity -> Maybe Literal
entityToLiteral (MkEntity anchor) = do
    bs <- anchorToByteString anchor
    return $ MkLiteral bs

instance AsLiteral Literal where
    literalSerializer = coerce serializerWhole

instance AsLiteral Void where
    literalSerializer = pNone

instance AsLiteral Text where
    -- [t]
    literalSerializer = pLiteral 0x74 ***> codecMap' utf8Codec serializerWhole

instance AsLiteral String where
    literalSerializer = isoMap unpack pack $ literalSerializer @Text

instance AsLiteral () where
    -- [u]
    literalSerializer = pLiteral 0x75

instance AsLiteral Bool where
    literalSerializer = pLiteral 0x62 ***> serializer

instance AsLiteral Ordering where
    -- [o]
    literalSerializer = pLiteral 0x6F ***> codecMap readShowCodec serializer

instance AsLiteral Number where
    literalSerializer = let
        eitherToNumber :: Either Rational Double -> Number
        eitherToNumber (Left x) = ExactNumber x
        eitherToNumber (Right x) = InexactNumber x
        numberToEither :: Number -> Either Rational Double
        numberToEither (ExactNumber x) = Left x
        numberToEither (InexactNumber x) = Right x
        in isoMap eitherToNumber numberToEither $ literalSerializer <+++> literalSerializer

instance AsLiteral Rational where
    -- [r]
    literalSerializer = pLiteral 0x72 ***> serializer

instance AsLiteral Double where
    -- [d]
    literalSerializer = pLiteral 0x64 ***> serializer

instance AsLiteral SafeRational where
    literalSerializer = codecMap safeRationalNumber literalSerializer

instance AsLiteral Integer where
    literalSerializer = codecMap integerSafeRational literalSerializer

instance AsLiteral Day where
    -- [Td]
    literalSerializer = pLiteralBytes [0x54, 0x64] ***> serializer

instance AsLiteral TimeOfDay where
    -- [To]
    literalSerializer = pLiteralBytes [0x54, 0x6F] ***> serializer

instance AsLiteral LocalTime where
    -- [Tl]
    literalSerializer = pLiteralBytes [0x54, 0x6C] ***> serializer

instance AsLiteral UTCTime where
    -- [Tu]
    literalSerializer = pLiteralBytes [0x54, 0x75] ***> serializer

instance AsLiteral NominalDiffTime where
    -- [Tn]
    literalSerializer = pLiteralBytes [0x54, 0x6E] ***> serializer
