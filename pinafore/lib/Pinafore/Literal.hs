module Pinafore.Literal where

import Data.Time
import Pinafore.Number
import Prelude (Rational)
import Shapes

newtype Literal = MkLiteral
    { unLiteral :: Text
    } deriving (Eq)

instance Show Literal where
    show (MkLiteral t) = show t

class Show t => AsLiteral t where
    toLiteral :: t -> Literal
    fromLiteral :: Literal -> Maybe t
    literalTypeDescription :: Text

instance AsLiteral Literal where
    toLiteral = id
    fromLiteral = Just
    literalTypeDescription = "literal"

instance AsLiteral Text where
    toLiteral = MkLiteral
    fromLiteral = Just . unLiteral
    literalTypeDescription = "text"

instance AsLiteral String where
    toLiteral = MkLiteral . pack
    fromLiteral = Just . unpack . unLiteral
    literalTypeDescription = "text"

instance AsLiteral () where
    toLiteral () = MkLiteral $ fromString "unit"
    fromLiteral (MkLiteral "unit") = Just ()
    fromLiteral _ = Nothing
    literalTypeDescription = "unit"

instance AsLiteral Bool where
    toLiteral True = MkLiteral $ fromString "true"
    toLiteral False = MkLiteral $ fromString "false"
    fromLiteral text = lookup text $ fmap (\t -> (toLiteral t, t)) allValues
    literalTypeDescription = "boolean"

instance AsLiteral Number where
    toLiteral = MkLiteral . pack . show
    fromLiteral = readMaybe . unpack . unLiteral
    literalTypeDescription = "number"

instance AsLiteral Int where
    toLiteral = MkLiteral . pack . show
    fromLiteral = readMaybe . unpack . unLiteral
    literalTypeDescription = "integer"

instance AsLiteral Int64 where
    toLiteral = MkLiteral . pack . show
    fromLiteral = readMaybe . unpack . unLiteral
    literalTypeDescription = "integer"

instance AsLiteral Integer where
    toLiteral = MkLiteral . pack . show
    fromLiteral = readMaybe . unpack . unLiteral
    literalTypeDescription = "integer"

instance AsLiteral Rational where
    toLiteral = toLiteral . ExactNumber
    fromLiteral t = do
        n <- fromLiteral t
        case n of
            ExactNumber x -> return x
            _ -> Nothing
    literalTypeDescription = "exact-number"

instance AsLiteral Double where
    toLiteral = toLiteral . InexactNumber
    fromLiteral t = do
        n <- fromLiteral t
        case n of
            InexactNumber x -> return x
            _ -> Nothing
    literalTypeDescription = "inexact-number"

instance AsLiteral Day where
    toLiteral = MkLiteral . pack . show
    fromLiteral = readMaybe . unpack . unLiteral
    literalTypeDescription = "day"

instance AsLiteral UTCTime where
    toLiteral = MkLiteral . pack . show
    fromLiteral = readMaybe . unpack . unLiteral
    literalTypeDescription = "time"
