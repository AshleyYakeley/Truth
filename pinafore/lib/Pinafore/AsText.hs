module Pinafore.AsText where

import Data.Time
import Pinafore.Number
import Shapes

class AsText t where
    toText :: t -> Text
    fromText :: Text -> Maybe t
    textTypeDescription :: Text

instance AsText Text where
    toText = id
    fromText = Just
    textTypeDescription = "text"

instance AsText String where
    toText = pack
    fromText = Just . unpack
    textTypeDescription = "text"

instance AsText Bool where
    toText True = fromString "true"
    toText False = fromString "false"
    fromText text = lookup text $ fmap (\t -> (toText t, t)) allValues
    textTypeDescription = "boolean"

instance AsText Int where
    toText = pack . show
    fromText = readMaybe . unpack
    textTypeDescription = "integer"

instance AsText Integer where
    toText = pack . show
    fromText = readMaybe . unpack
    textTypeDescription = "integer"

instance AsText Double where
    toText = pack . show
    fromText = readMaybe . unpack
    textTypeDescription = "number"

instance AsText Number where
    toText = pack . show
    fromText = readMaybe . unpack
    textTypeDescription = "number"

instance AsText Day where
    toText = pack . show
    fromText = readMaybe . unpack
    textTypeDescription = "day"

instance AsText UTCTime where
    toText = pack . show
    fromText = readMaybe . unpack
    textTypeDescription = "time"
