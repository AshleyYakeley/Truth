module Pinafore.Base.MIME
    ( MIME(..)
    , plainTextMIMEType
    , octetStreamMIMEType
    , vndMIMEType
    , mimeToText
    , textToMIME
    ) where

import Changes.World.MIME
import Shapes

data MIME = MkMIME
    { mimeType :: MIMEContentType
    , mimeContent :: StrictByteString
    }

plainTextMIMEType :: MIMEContentType
plainTextMIMEType = MkMIMEContentType TextMimeType "plain" [("charset", "utf-8")]

octetStreamMIMEType :: MIMEContentType
octetStreamMIMEType = MkMIMEContentType ApplicationMimeType "octet-stream" []

vndMIMEType :: Text -> MIMEContentType
vndMIMEType t = MkMIMEContentType ApplicationMimeType ("vnd.pinafore." <> t) []

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
isTextType TextMimeType _ = True
isTextType ApplicationMimeType "xml" = True
isTextType ApplicationMimeType s = isJust $ endsWith "+xml" $ unpack s
isTextType _ _ = False

mimeToText :: MIME -> Maybe Text
mimeToText (MkMIME (MkMIMEContentType t s p) b) = do
    altIf $ isTextType t s
    encoding <- getCharsetEncoding $ lookup "charset" p
    encoding b

textToMIME :: Text -> MIME
textToMIME t = MkMIME plainTextMIMEType $ encode utf8Codec t
