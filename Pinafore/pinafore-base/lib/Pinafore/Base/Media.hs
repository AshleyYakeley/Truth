module Pinafore.Base.Media
    ( Media(..)
    , plainTextMediaType
    , octetStreamMediaType
    , vndMediaType
    , mediaText
    ) where

import Changes.World.Media.Type
import Shapes

data Media = MkMedia
    { mediaType :: MediaType
    , mediaContent :: StrictByteString
    }

plainTextMediaType :: MediaType
plainTextMediaType = MkMediaType TextMediaType "plain" [("charset", "utf-8")]

octetStreamMediaType :: MediaType
octetStreamMediaType = MkMediaType ApplicationMediaType "octet-stream" []

vndMediaType :: Text -> MediaType
vndMediaType t = MkMediaType ApplicationMediaType ("vnd.pinafore." <> t) []

-- not quite correct, but good enough
decodeASCII :: StrictByteString -> Maybe Text
decodeASCII = eitherRight . decodeUtf8'

getCharsetEncoding :: Maybe Text -> Maybe (StrictByteString -> Maybe Text)
getCharsetEncoding Nothing = Just decodeASCII
getCharsetEncoding (Just cs) =
    case toLower cs of
        "utf-8" -> Just $ eitherRight . decodeUtf8'
        "iso-8859-1" -> Just $ Just . decodeLatin1
        "us-ascii" -> Just decodeASCII
        _ -> Nothing

isTextType :: Text -> Text -> Bool
isTextType TextMediaType _ = True
isTextType ApplicationMediaType "xml" = True
isTextType ApplicationMediaType s = isJust $ endsWith "+xml" $ unpack s
isTextType _ _ = False

mediaText :: Codec Media Text
mediaText =
    MkCodec
        { decode =
              \(MkMedia (MkMediaType t s p) b) -> do
                  altIf $ isTextType t s
                  encoding <- getCharsetEncoding $ lookup "charset" p
                  encoding b
        , encode = \t -> MkMedia plainTextMediaType $ encode utf8Codec t
        }
