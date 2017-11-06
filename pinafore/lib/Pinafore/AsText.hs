module Pinafore.AsText where

import Data.Time
import Shapes

class AsText t where
    toText :: t -> Text
    fromText :: Text -> Maybe t

instance AsText Text where
    toText = id
    fromText = Just

instance AsText String where
    toText = pack
    fromText = Just . unpack

instance AsText Bool where
    toText True = fromString "true"
    toText False = fromString "false"
    fromText text = lookup text $ fmap (\t -> (toText t, t)) allValues

instance AsText Int where
    toText = pack . show
    fromText = readMaybe . unpack

instance AsText Integer where
    toText = pack . show
    fromText = readMaybe . unpack

instance AsText Double where
    toText = pack . show
    fromText = readMaybe . unpack

instance AsText Day where
    toText = pack . show
    fromText = readMaybe . unpack

instance AsText UTCTime where
    toText = pack . show
    fromText = readMaybe . unpack
