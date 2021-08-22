module Pinafore.Base.Literal where

import Data.Time
import Pinafore.Base.Entity
import Pinafore.Base.Know
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

fromLiteral :: AsLiteral t => Literal -> Know t
fromLiteral lit = maybeToKnow $ decode literalCodec lit

literalToEntity :: AsLiteral t => t -> Entity
literalToEntity v = hashToEntity $ \call -> [call @Text "literal:", call $ toLiteral v]

instance AsLiteral Literal where
    literalSerializer = coerce serializerWhole

instance AsLiteral None where
    literalSerializer = pNone

instance AsLiteral Text where
    literalSerializer = pLiteral 0x74 ***> serializer

instance AsLiteral String where
    literalSerializer = isoMap unpack pack $ literalSerializer @Text

instance AsLiteral () where
    literalSerializer = pLiteral 0x75

instance AsLiteral Bool where
    literalSerializer = pLiteral 0x62 ***> serializer

instance AsLiteral Ordering where
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
    literalSerializer = pLiteral 0x72 ***> serializer

instance AsLiteral Double where
    literalSerializer = pLiteral 0x64 ***> serializer

instance AsLiteral SafeRational where
    literalSerializer = codecMap safeRationalNumber literalSerializer

instance AsLiteral Integer where
    literalSerializer = codecMap integerSafeRational literalSerializer

{-
instance AsLiteral Int where
    toLiteral = MkLiteral . pack . show
    fromLiteral = readFromLiteral

instance AsLiteral Int64 where
    toLiteral = MkLiteral . pack . show
    fromLiteral = readFromLiteral

instance HasResolution r => AsLiteral (Fixed r) where
    toLiteral = MkLiteral . pack . show
    fromLiteral = readFromLiteral
-}
instance AsLiteral Day where
    literalSerializer = pLiteralBytes [0x54, 0x64] ***> serializer

instance AsLiteral TimeOfDay where
    literalSerializer = pLiteralBytes [0x54, 0x6F] ***> serializer

instance AsLiteral LocalTime where
    literalSerializer = pLiteralBytes [0x54, 0x6C] ***> serializer

instance AsLiteral UTCTime where
    literalSerializer = pLiteralBytes [0x54, 0x75] ***> serializer

instance AsLiteral NominalDiffTime where
    literalSerializer = pLiteralBytes [0x54, 0x6E] ***> serializer
