module Pinafore.Base.Literal
    ( Literal(..)
    , AsLiteral(..)
    , pattern MkMIMELiteral
    , literalCodec
    , toLiteral
    , fromLiteral
    , literalToEntity
    , entityToLiteral
    ) where

import Changes.World.MIME
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
    default literalSerializer :: AsMIMELiteral t => Serializer t
    literalSerializer = pLiteralBytes (literalByteCode @t) ***> literalContentSerializer

class AsLiteral t => AsMIMELiteral t where
    literalMimeType :: MIMEContentType
    literalByteCode :: [Word8]
    literalContentSerializer :: Serializer t

mkMimeLiteralDict ::
       forall t. AsMIMELiteral t
    => Some (Compose Dict AsMIMELiteral)
mkMimeLiteralDict = MkSome $ Compose $ Dict @(AsMIMELiteral t)

mimeLiteralDicts :: [Some (Compose Dict AsMIMELiteral)]
mimeLiteralDicts =
    [ mkMimeLiteralDict @Text
    , mkMimeLiteralDict @()
    , mkMimeLiteralDict @Bool
    , mkMimeLiteralDict @Ordering
    , mkMimeLiteralDict @Rational
    , mkMimeLiteralDict @Double
    , mkMimeLiteralDict @Day
    , mkMimeLiteralDict @TimeOfDay
    , mkMimeLiteralDict @LocalTime
    , mkMimeLiteralDict @UTCTime
    , mkMimeLiteralDict @NominalDiffTime
    ]

mimeSerializer :: Serializer (MIMEContentType, StrictByteString)
-- [m]
mimeSerializer = pLiteralBytes [0x6D] ***> serializer <***> pWhole

literalToMIME :: Literal -> Maybe (MIMEContentType, StrictByteString)
literalToMIME (MkLiteral bs) = let
    checkDict :: forall t. Compose Dict AsMIMELiteral t -> Maybe (MIMEContentType, StrictByteString)
    checkDict (Compose Dict) = do
        b <- serializerStrictDecode (pLiteralBytes (literalByteCode @t) ***> pWhole) bs
        return (literalMimeType @t, b)
    in case mapMaybe (\(MkSome cd) -> checkDict cd) mimeLiteralDicts of
           tb:_ -> Just tb
           [] -> serializerStrictDecode mimeSerializer bs

mimeToLiteral :: MIMEContentType -> StrictByteString -> Literal
mimeToLiteral t b = let
    checkDict :: forall t. Compose Dict AsMIMELiteral t -> Maybe Literal
    checkDict (Compose Dict) =
        if t == literalMimeType @t
            then Just $ MkLiteral $ serializerStrictEncode (pLiteralBytes (literalByteCode @t) ***> pWhole) b
            else Nothing
    in case mapMaybe (\(MkSome cd) -> checkDict cd) mimeLiteralDicts of
           l:_ -> l
           [] -> MkLiteral $ serializerStrictEncode mimeSerializer (t, b)

pattern MkMIMELiteral ::
        MIMEContentType -> StrictByteString -> Literal

pattern MkMIMELiteral t b <- (literalToMIME -> Just (t, b))
  where MkMIMELiteral t b = mimeToLiteral t b

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
    literalSerializer = coerce $ pWhole @Serializer

instance AsLiteral Void where
    literalSerializer = pNone

instance AsLiteral Text

instance AsMIMELiteral Text where
    literalMimeType = MkMIMEContentType TextMimeType "plain" [("charset", "utf-8")]
    -- [t]
    literalByteCode = [0x74]
    literalContentSerializer = codecMap' utf8Codec pWhole

instance AsLiteral String where
    literalSerializer = invmap unpack pack $ literalSerializer @Text

instance AsLiteral ()

instance AsMIMELiteral () where
    literalMimeType = MkMIMEContentType ApplicationMimeType "vnd.pinafore.unit" []
    -- [u]
    literalByteCode = [0x75]
    literalContentSerializer = pUnit

instance AsLiteral Bool

instance AsMIMELiteral Bool where
    literalMimeType = MkMIMEContentType ApplicationMimeType "vnd.pinafore.boolean" []
    -- [b]
    literalByteCode = [0x62]
    literalContentSerializer = serializer

instance AsLiteral Ordering

instance AsMIMELiteral Ordering where
    literalMimeType = MkMIMEContentType ApplicationMimeType "vnd.pinafore.ordering" []
    -- [o]
    literalByteCode = [0x6F]
    literalContentSerializer = codecMap readShowCodec serializer

instance AsLiteral Number where
    literalSerializer = let
        eitherToNumber :: Either Rational Double -> Number
        eitherToNumber (Left x) = ExactNumber x
        eitherToNumber (Right x) = InexactNumber x
        numberToEither :: Number -> Either Rational Double
        numberToEither (ExactNumber x) = Left x
        numberToEither (InexactNumber x) = Right x
        in invmap eitherToNumber numberToEither $ literalSerializer <+++> literalSerializer

instance AsLiteral Rational

instance AsMIMELiteral Rational where
    literalMimeType = MkMIMEContentType ApplicationMimeType "vnd.pinafore.rational" []
    -- [r]
    literalByteCode = [0x72]
    literalContentSerializer = serializer

instance AsLiteral Double

instance AsMIMELiteral Double where
    literalMimeType = MkMIMEContentType ApplicationMimeType "vnd.pinafore.double" []
    -- [d]
    literalByteCode = [0x64]
    literalContentSerializer = serializer

instance AsLiteral SafeRational where
    literalSerializer = codecMap safeRationalNumber literalSerializer

instance AsLiteral Integer where
    literalSerializer = codecMap integerSafeRational literalSerializer

instance AsLiteral Day

instance AsMIMELiteral Day where
    literalMimeType = MkMIMEContentType ApplicationMimeType "vnd.pinafore.day" []
    -- [Td]
    literalByteCode = [0x54, 0x64]
    literalContentSerializer = serializer

instance AsLiteral TimeOfDay

instance AsMIMELiteral TimeOfDay where
    literalMimeType = MkMIMEContentType ApplicationMimeType "vnd.pinafore.timeofday" []
    -- [To]
    literalByteCode = [0x54, 0x6F]
    literalContentSerializer = serializer

instance AsLiteral LocalTime

instance AsMIMELiteral LocalTime where
    literalMimeType = MkMIMEContentType ApplicationMimeType "vnd.pinafore.localtime" []
    -- [Tl]
    literalByteCode = [0x54, 0x6C]
    literalContentSerializer = serializer

instance AsLiteral UTCTime

instance AsMIMELiteral UTCTime where
    literalMimeType = MkMIMEContentType ApplicationMimeType "vnd.pinafore.time" []
    -- [Tu]
    literalByteCode = [0x54, 0x75]
    literalContentSerializer = serializer

instance AsLiteral NominalDiffTime

instance AsMIMELiteral NominalDiffTime where
    literalMimeType = MkMIMEContentType ApplicationMimeType "vnd.pinafore.duration" []
    -- [Tn]
    literalByteCode = [0x54, 0x6E]
    literalContentSerializer = serializer
