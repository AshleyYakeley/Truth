module Changes.World.Media.Type
    ( MediaType
    , pattern MkMediaType
    , mtType
    , mtSubtype
    , mtParams
    , pattern TextMediaType
    , pattern ImageMediaType
    , pattern ApplicationMediaType
    ) where

import Shapes

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
    serializer = let
        toMT :: (Text, (Text, [(Text, Text)])) -> MediaType
        toMT (t, (s, p)) = MkMediaType_ t s p
        fromMT :: MediaType -> (Text, (Text, [(Text, Text)]))
        fromMT (MkMediaType_ t s p) = (t, (s, p))
        in invmap toMT fromMT $ serializer <***> serializer <***> rList (serializer <***> serializer)

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

instance Show MediaType where
    show MkMediaType_ {..} =
        unpack mtType <>
        "/" <> unpack mtSubtype <> mconcat (fmap (\(n, v) -> ";" <> unpack n <> "=" <> enc (unpack v)) mtParams)

pattern TextMediaType :: Text

pattern TextMediaType = "text"

pattern ImageMediaType :: Text

pattern ImageMediaType = "image"

pattern ApplicationMediaType :: Text

pattern ApplicationMediaType = "application"

instance HasNewValue MediaType where
    newValue = MkMediaType_ ApplicationMediaType "octet-stream" []
