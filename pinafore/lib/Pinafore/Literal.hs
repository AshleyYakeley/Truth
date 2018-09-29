module Pinafore.Literal where

import Data.Time
import Pinafore.Know
import Pinafore.Number
import Prelude (Rational)
import Shapes

newtype Literal = MkLiteral
    { unLiteral :: Text
    } deriving (Eq)

instance Show Literal where
    show (MkLiteral t) = show t

readFromLiteral :: Read t => Literal -> Know t
readFromLiteral = maybeToKnow . readMaybe . unpack . unLiteral

class (Eq t, Show t) => AsLiteral t where
    toLiteral :: t -> Literal
    fromLiteral :: Literal -> Know t
    literalTypeDescription :: Text

instance AsLiteral Literal where
    toLiteral = id
    fromLiteral = Known
    literalTypeDescription = "literal"

instance AsLiteral None where
    toLiteral = never
    fromLiteral _ = Unknown
    literalTypeDescription = "none"

instance AsLiteral Text where
    toLiteral = MkLiteral
    fromLiteral = Known . unLiteral
    literalTypeDescription = "text"

instance AsLiteral String where
    toLiteral = MkLiteral . pack
    fromLiteral = Known . unpack . unLiteral
    literalTypeDescription = "text"

instance AsLiteral () where
    toLiteral () = MkLiteral $ fromString "unit"
    fromLiteral (MkLiteral "unit") = Known ()
    fromLiteral _ = Unknown
    literalTypeDescription = "unit"

instance AsLiteral Bool where
    toLiteral True = MkLiteral $ fromString "true"
    toLiteral False = MkLiteral $ fromString "false"
    fromLiteral text = maybeToKnow $ lookup text $ fmap (\t -> (toLiteral t, t)) allValues
    literalTypeDescription = "boolean"

instance AsLiteral Number where
    toLiteral = MkLiteral . pack . show
    fromLiteral = readFromLiteral
    literalTypeDescription = "number"

instance AsLiteral Int where
    toLiteral = MkLiteral . pack . show
    fromLiteral = readFromLiteral
    literalTypeDescription = "integer"

instance AsLiteral Int64 where
    toLiteral = MkLiteral . pack . show
    fromLiteral = readFromLiteral
    literalTypeDescription = "integer"

instance AsLiteral Integer where
    toLiteral = MkLiteral . pack . show
    fromLiteral = readFromLiteral
    literalTypeDescription = "integer"

instance AsLiteral Rational where
    toLiteral = toLiteral . ExactNumber
    fromLiteral t = do
        n <- fromLiteral t
        case n of
            ExactNumber x -> return x
            _ -> Unknown
    literalTypeDescription = "exact-number"

instance AsLiteral Double where
    toLiteral = toLiteral . InexactNumber
    fromLiteral t = do
        n <- fromLiteral t
        case n of
            InexactNumber x -> return x
            _ -> Unknown
    literalTypeDescription = "inexact-number"

instance AsLiteral Day where
    toLiteral = MkLiteral . pack . show
    fromLiteral = readFromLiteral
    literalTypeDescription = "day"

instance AsLiteral UTCTime where
    toLiteral = MkLiteral . pack . show
    fromLiteral = readFromLiteral
    literalTypeDescription = "time"
