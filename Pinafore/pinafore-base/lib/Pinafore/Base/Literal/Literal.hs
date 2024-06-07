module Pinafore.Base.Literal.Literal
    ( Literal(..)
    , AsLiteral(..)
    , toLiteral
    , fromLiteral
    , literalToEntity
    , entityToLiteral
    , AsTypedLiteral(..)
    ) where

import Changes.World.Media.Type
import Data.Time
import Pinafore.Base.Anchor
import Pinafore.Base.Entity
import Pinafore.Base.Literal.Type
import Pinafore.Base.Number
import Pinafore.Base.SafeRational
import Shapes
import Shapes.Numeric

newtype Literal = MkLiteral
    { unLiteral :: StrictByteString
    } deriving newtype (Eq, HasSerializer)

instance Show Literal where
    show (MkLiteral t) = show t

class Eq t => AsLiteral t where
    literalCodec :: Codec Literal t
    default literalCodec :: AsTypedLiteral t => Codec Literal t
    literalCodec = serializerStrictCodec literalSerializer . coerceCodec

toLiteral :: AsLiteral t => t -> Literal
toLiteral = encode literalCodec

fromLiteral :: AsLiteral t => Literal -> Maybe t
fromLiteral = decode literalCodec

literalToEntity :: AsLiteral t => t -> Entity
literalToEntity v = MkEntity $ byteStringToAnchor $ unLiteral $ toLiteral v

class AsLiteral t => AsTypedLiteral t where
    literalType :: LiteralType
    literalContentSerializer :: Serializer KeepsGoing t
    default literalContentSerializer :: HasSerializer t => Serializer KeepsGoing t
    literalContentSerializer = greedySerializer

literalSerializer ::
       forall t. AsTypedLiteral t
    => Serializer KeepsGoing t
literalSerializer = sProductR (runLiteralType $ literalType @t) literalContentSerializer

entityToLiteral :: Entity -> Maybe Literal
entityToLiteral (MkEntity anchor) = do
    bs <- anchorToByteString anchor
    return $ MkLiteral bs

instance AsLiteral Literal where
    literalCodec = id

instance AsLiteral MediaType

instance AsTypedLiteral MediaType where
    literalType = mediaTypeLiteralType

instance AsLiteral Void where
    literalCodec = rVoid

instance AsLiteral StrictByteString

instance AsTypedLiteral StrictByteString where
    literalType = blobLiteralType

instance AsLiteral Text

instance AsTypedLiteral Text where
    literalType = textLiteralType
    literalContentSerializer = codecMap' utf8Codec sWhole

instance AsLiteral String where
    literalCodec = bijectionCodec unpackBijection . literalCodec @Text

instance AsLiteral ()

instance AsTypedLiteral () where
    literalType = unitLiteralType

instance AsLiteral Bool

instance AsTypedLiteral Bool where
    literalType = booleanLiteralType

instance AsLiteral Ordering

instance AsTypedLiteral Ordering where
    literalType = orderingLiteralType

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

instance AsTypedLiteral Rational where
    literalType = rationalLiteralType

instance AsLiteral Double

instance AsTypedLiteral Double where
    literalType = doubleLiteralType

instance AsLiteral SafeRational where
    literalCodec = codecMap safeRationalNumber literalCodec

instance AsLiteral Integer where
    literalCodec = codecMap integerSafeRational literalCodec

instance AsLiteral Day

instance AsTypedLiteral Day where
    literalType = dayLiteralType

instance AsLiteral TimeOfDay

instance AsTypedLiteral TimeOfDay where
    literalType = timeOfDayLiteralType

instance AsLiteral LocalTime

instance AsTypedLiteral LocalTime where
    literalType = localTimeLiteralType

instance AsLiteral UTCTime

instance AsTypedLiteral UTCTime where
    literalType = timeLiteralType

instance AsLiteral NominalDiffTime

instance AsTypedLiteral NominalDiffTime where
    literalType = durationLiteralType
