module Pinafore.AsText where

import Data.Time
import Pinafore.Number
import Prelude (Rational)
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

instance AsText () where
    toText () = fromString "unit"
    fromText "unit" = Just ()
    fromText _ = Nothing
    textTypeDescription = "unit"

instance AsText Bool where
    toText True = fromString "true"
    toText False = fromString "false"
    fromText text = lookup text $ fmap (\t -> (toText t, t)) allValues
    textTypeDescription = "boolean"

instance AsText Number where
    toText = pack . show
    fromText = readMaybe . unpack
    textTypeDescription = "number"

instance AsText Int where
    toText = pack . show
    fromText = readMaybe . unpack
    textTypeDescription = "integer"

instance AsText Integer where
    toText = pack . show
    fromText = readMaybe . unpack
    textTypeDescription = "integer"

instance AsText Rational where
    toText = toText . ExactNumber
    fromText t = do
        n <- fromText t
        case n of
            ExactNumber x -> return x
            _ -> Nothing
    textTypeDescription = "exact-number"

instance AsText Double where
    toText = toText . InexactNumber
    fromText t = do
        n <- fromText t
        case n of
            InexactNumber x -> return x
            _ -> Nothing
    textTypeDescription = "inexact-number"

instance AsText Day where
    toText = pack . show
    fromText = readMaybe . unpack
    textTypeDescription = "day"

instance AsText UTCTime where
    toText = pack . show
    fromText = readMaybe . unpack
    textTypeDescription = "time"
