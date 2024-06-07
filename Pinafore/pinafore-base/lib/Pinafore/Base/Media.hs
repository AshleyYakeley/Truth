module Pinafore.Base.Media
    ( Media(..)
    , octetStreamMediaType
    , mediaText
    , mediaSpecificText
    ) where

import Changes.World.Media.Type
import Shapes

data Media = MkMedia
    { mediaType :: MediaType
    , mediaContent :: StrictByteString
    } deriving (Eq)

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

octetStreamMediaType :: MediaType
octetStreamMediaType = MkMediaType ApplicationMediaType "octet-stream" []

decodeUTF8 :: StrictByteString -> Maybe Text
decodeUTF8 = eitherRight . decodeUtf8'

-- not quite correct, but good enough
decodeASCII :: StrictByteString -> Maybe Text
decodeASCII = decodeUTF8

getMediaTextEncoding :: MediaType -> Maybe (StrictByteString -> Maybe Text)
getMediaTextEncoding (MkMediaType TextMediaType _ p) =
    case fmap toLower $ lookup "charset" p of
        Nothing -> Just decodeASCII
        Just "utf-8" -> Just decodeUTF8
        Just "iso-8859-1" -> Just $ Just . decodeLatin1
        Just "us-ascii" -> Just decodeASCII
        Just _ -> Nothing
getMediaTextEncoding (MkMediaType ApplicationMediaType "xml" _) = Just decodeUTF8
getMediaTextEncoding (MkMediaType ApplicationMediaType s _)
    | isJust $ endsWith "+xml" $ unpack s = Just decodeUTF8
getMediaTextEncoding (MkMediaType ApplicationMediaType "html" _) = Just decodeUTF8
getMediaTextEncoding _ = Nothing

mediaSpecificText :: (Text, Text) -> ((Text, Text) -> Bool) -> Codec Media Text
mediaSpecificText (pt, ps) f = let
    pp =
        case pt of
            TextMediaType -> [("charset", "utf-8")]
            _ -> []
    pmt :: MediaType
    pmt = MkMediaType pt ps pp
    ee :: Text -> Media
    ee text = MkMedia pmt $ encode utf8Codec text
    mtf :: MediaType -> Bool
    mtf (MkMediaType t s _) = f (t, s)
    dd :: Media -> Maybe Text
    dd (MkMedia mt b) = do
        altIf $ mtf mt
        encoding <- getMediaTextEncoding mt
        encoding b
    in MkCodec dd ee

mediaText :: Codec Media Text
mediaText = mediaSpecificText (TextMediaType, "plain") (\_ -> True)
