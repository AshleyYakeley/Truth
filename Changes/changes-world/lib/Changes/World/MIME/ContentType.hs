module Changes.World.MIME.ContentType
    ( MIMEContentType
    , pattern MkMIMEContentType
    , mctType
    , mctSubtype
    , mctParams
    , pattern TextMimeType
    , pattern ImageMimeType
    , pattern ApplicationMimeType
    ) where

import Shapes

data MIMEContentType = MkMIMEContentType_
    { mctType :: Text
    , mctSubtype :: Text
    , mctParams :: [(Text, Text)]
    } deriving (Eq)

pattern MkMIMEContentType ::
        Text -> Text -> [(Text, Text)] -> MIMEContentType

pattern MkMIMEContentType t s p <- MkMIMEContentType_ t s p
  where MkMIMEContentType t s p
          = MkMIMEContentType_ (toLower t) (toLower s)
              (fmap (\ (n, v) -> (toLower n, v)) p)

instance HasSerializer MIMEContentType where
    serializer = let
        toMCT :: (Text, (Text, [(Text, Text)])) -> MIMEContentType
        toMCT (t, (s, p)) = MkMIMEContentType_ t s p
        fromMCT :: MIMEContentType -> (Text, (Text, [(Text, Text)]))
        fromMCT (MkMIMEContentType_ t s p) = (t, (s, p))
        in invmap toMCT fromMCT $ serializer <***> serializer <***> rList (serializer <***> serializer)

goodchar :: Char -> Bool
goodchar c
    | elem c ("()<>@,;:\\\"/[]?=" :: String) = False
goodchar c
    | isControl c = False
goodchar c
    | isSpace c = False
goodchar c = isAscii c

enc :: String -> String
enc s =
    if all goodchar s
        then s
        else show s

instance Show MIMEContentType where
    show MkMIMEContentType_ {..} =
        unpack mctType <>
        "/" <> unpack mctSubtype <> mconcat (fmap (\(n, v) -> ";" <> unpack n <> "=" <> enc (unpack v)) mctParams)

pattern TextMimeType :: Text

pattern TextMimeType = "text"

pattern ImageMimeType :: Text

pattern ImageMimeType = "image"

pattern ApplicationMimeType :: Text

pattern ApplicationMimeType = "application"

instance HasNewValue MIMEContentType where
    newValue = MkMIMEContentType_ ApplicationMimeType "octet-stream" []
