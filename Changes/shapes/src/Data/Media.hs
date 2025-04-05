module Data.Media
    ( MediaType
    , pattern MkMediaType
    , mtType
    , mtSubtype
    , mtParams
    , textMediaTypeCodec
    , pattern TextMediaType
    , pattern ImageMediaType
    , pattern ApplicationMediaType
    , Media (..)
    , showMediaType
    )
where

import Data.Codec
import Data.HasNewValue
import Data.Serialize.Has
import Data.Serializer
import Data.Streamable
import Shapes.Import

data MediaType = MkMediaType_
    { mtType :: Text
    , mtSubtype :: Text
    , mtParams :: [(Text, Text)]
    }
    deriving stock Eq

pattern MkMediaType :: Text -> Text -> [(Text, Text)] -> MediaType
pattern MkMediaType t s p <- MkMediaType_ t s p
    where
        MkMediaType t s p =
            MkMediaType_
                (toLower t)
                (toLower s)
                (fmap (\(n, v) -> (toLower n, v)) p)

{-# COMPLETE MkMediaType #-}

instance HasSerializer MediaType where
    stoppingSerializer = let
        toMT :: (Text, (Text, [(Text, Text)])) -> MediaType
        toMT (t, (s, p)) = MkMediaType_ t s p
        fromMT :: MediaType -> (Text, (Text, [(Text, Text)]))
        fromMT (MkMediaType_ t s p) = (t, (s, p))
        in invmap toMT fromMT
            $ sProduct stoppingSerializer
            $ sProduct stoppingSerializer
            $ sCountedList
            $ sProduct stoppingSerializer stoppingSerializer

-- RFC 2045 sec. 5.1
goodchar :: Char -> Bool
goodchar c
    | elem c ("()<>@,;:\\\"/[]?=" :: String) = False
goodchar c
    | isControl c = False
goodchar c
    | isSpace c = False
goodchar c = isAscii c

enc :: Text -> Text
enc s =
    if all goodchar $ unpack s
        then s
        else pack $ show s

parseThis :: String -> ReadPrec ()
parseThis s = do
    rLiterals s
    rSkipSpaces

parseToken :: ReadPrec String
parseToken = do
    s <- rSome $ rSatisfy goodchar
    rSkipSpaces
    return s

parseQSC :: ReadPrec Char
parseQSC = do
    c <- rItem
    case c of
        '"' -> mzero
        '\\' -> rItem
        _ -> return c

parseQS :: ReadPrec String
parseQS = do
    _ <- rLiteral '"'
    s <- many parseQSC
    _ <- rLiteral '"'
    return s

parseValue :: ReadPrec String
parseValue = parseQS <|> parseToken

-- RFC 2045 sec. 5.1
parseMediaType :: ReadPrec MediaType
parseMediaType = do
    rSkipSpaces
    tt <- parseToken
    parseThis "/"
    st <- parseToken
    mp <-
        many $ do
            parseThis ";"
            n <- parseToken
            parseThis "="
            v <- parseValue
            return (pack n, pack v)
    return $ MkMediaType (pack tt) (pack st) mp

showMediaType :: MediaType -> Text
showMediaType MkMediaType_{..} = mtType <> "/" <> mtSubtype <> concatmap (\(n, v) -> ";" <> n <> "=" <> enc v) mtParams

textMediaTypeCodec :: Codec Text MediaType
textMediaTypeCodec = let
    decode t = runReadPrec parseMediaType $ unpack t
    encode = showMediaType
    in MkCodec{..}

instance Read MediaType where
    readPrec = parseMediaType

instance Show MediaType where
    show mt = unpack $ showMediaType mt

pattern TextMediaType :: Text
pattern TextMediaType = "text"

pattern ImageMediaType :: Text
pattern ImageMediaType = "image"

pattern ApplicationMediaType :: Text
pattern ApplicationMediaType = "application"

instance HasNewValue MediaType where
    newValue = MkMediaType_ ApplicationMediaType "octet-stream" []

data Media = MkMedia
    { mediaType :: MediaType
    , mediaContent :: StrictByteString
    }
    deriving stock Eq

instance HasSerializer Media where
    greedySerializer = let
        toMedia :: (MediaType, StrictByteString) -> Media
        toMedia (t, b) = MkMedia t b
        fromMedia :: Media -> (MediaType, StrictByteString)
        fromMedia (MkMedia t b) = (t, b)
        in invmap toMedia fromMedia $ sProduct stoppingSerializer greedySerializer
    stoppingSerializer = let
        toMedia :: (MediaType, StrictByteString) -> Media
        toMedia (t, b) = MkMedia t b
        fromMedia :: Media -> (MediaType, StrictByteString)
        fromMedia (MkMedia t b) = (t, b)
        in invmap toMedia fromMedia $ sProduct stoppingSerializer stoppingSerializer
