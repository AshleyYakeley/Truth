module Changes.World.Media.Type
    ( MediaType
    , pattern MkMediaType
    , mtType
    , mtSubtype
    , mtParams
    , textMediaTypeCodec
    , pattern TextMediaType
    , pattern ImageMediaType
    , pattern ApplicationMediaType
    ) where

import Shapes
import qualified Text.ParserCombinators.ReadP as P

data MediaType = MkMediaType_
    { mtType :: Text
    , mtSubtype :: Text
    , mtParams :: [(Text, Text)]
    } deriving (Eq)

pattern MkMediaType :: Text -> Text -> [(Text, Text)] -> MediaType

pattern MkMediaType t s p <- MkMediaType_ t s p
  where MkMediaType t s p
          = MkMediaType_ (toLower t) (toLower s)
              (fmap (\ (n, v) -> (toLower n, v)) p)

{-# COMPLETE MkMediaType #-}

instance HasSerializer MediaType where
    stoppingSerializer = let
        toMT :: (Text, (Text, [(Text, Text)])) -> MediaType
        toMT (t, (s, p)) = MkMediaType_ t s p
        fromMT :: MediaType -> (Text, (Text, [(Text, Text)]))
        fromMT (MkMediaType_ t s p) = (t, (s, p))
        in invmap toMT fromMT $
           sProduct stoppingSerializer $
           sProduct stoppingSerializer $ sCountedList $ sProduct stoppingSerializer stoppingSerializer

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

parseThis :: String -> P.ReadP ()
parseThis s = do
    _ <- P.string s
    P.skipSpaces
    return ()

parseToken :: P.ReadP String
parseToken = do
    s <- P.munch1 goodchar
    P.skipSpaces
    return s

parseQSC :: P.ReadP Char
parseQSC = do
    c <- P.get
    case c of
        '"' -> mzero
        '\\' -> P.get
        _ -> return c

parseQS :: P.ReadP String
parseQS = do
    _ <- P.char '"'
    s <- many parseQSC
    _ <- P.char '"'
    return s

parseValue :: P.ReadP String
parseValue = parseQS <|> parseToken

-- RFC 2045 sec. 5.1
parseMediaType :: P.ReadP MediaType
parseMediaType = do
    P.skipSpaces
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
showMediaType MkMediaType_ {..} = mtType <> "/" <> mtSubtype <> concatmap (\(n, v) -> ";" <> n <> "=" <> enc v) mtParams

textMediaTypeCodec :: Codec Text MediaType
textMediaTypeCodec = let
    decode t =
        case P.readP_to_S parseMediaType $ unpack t of
            [(m, "")] -> return m
            _ -> Nothing
    encode = showMediaType
    in MkCodec {..}

instance Read MediaType where
    readsPrec _ = P.readP_to_S parseMediaType

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
