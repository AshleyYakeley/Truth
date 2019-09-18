module Pinafore.Base.Literal where

import Data.Time
import Pinafore.Base.Entity
import Pinafore.Base.Know
import Pinafore.Base.Number
import Pinafore.Base.SafeRational
import Shapes
import Shapes.Numeric

newtype Literal = MkLiteral
    { unLiteral :: Text
    } deriving (Eq, Serialize)

instance Show Literal where
    show (MkLiteral t) = show t

readFromLiteral :: Read t => Literal -> Know t
readFromLiteral = maybeToKnow . readMaybe . unpack . unLiteral

class (Eq t, Show t) => AsLiteral t where
    toLiteral :: t -> Literal
    fromLiteral :: Literal -> Know t

literalToEntity :: AsLiteral t => t -> Entity
literalToEntity v = hashToEntity $ \call -> [call @Text "literal:", call $ toLiteral v]

instance AsLiteral Literal where
    toLiteral = id
    fromLiteral = Known

instance AsLiteral None where
    toLiteral = never
    fromLiteral _ = Unknown

instance AsLiteral Text where
    toLiteral = MkLiteral
    fromLiteral = Known . unLiteral

instance AsLiteral String where
    toLiteral = MkLiteral . pack
    fromLiteral = Known . unpack . unLiteral

instance AsLiteral () where
    toLiteral () = MkLiteral $ fromString "unit"
    fromLiteral (MkLiteral "unit") = Known ()
    fromLiteral _ = Unknown

instance AsLiteral Bool where
    toLiteral True = MkLiteral $ fromString "True"
    toLiteral False = MkLiteral $ fromString "False"
    fromLiteral text = maybeToKnow $ lookup text $ fmap (\t -> (toLiteral t, t)) allValues

instance AsLiteral Number where
    toLiteral = MkLiteral . pack . show
    fromLiteral = readFromLiteral

instance AsLiteral Int where
    toLiteral = MkLiteral . pack . show
    fromLiteral = readFromLiteral

instance AsLiteral Int64 where
    toLiteral = MkLiteral . pack . show
    fromLiteral = readFromLiteral

instance AsLiteral Integer where
    toLiteral = MkLiteral . pack . show
    fromLiteral = readFromLiteral

instance HasResolution r => AsLiteral (Fixed r) where
    toLiteral = MkLiteral . pack . show
    fromLiteral = readFromLiteral

instance AsLiteral Rational where
    toLiteral = toLiteral . SRNumber
    fromLiteral t = do
        n <- fromLiteral t
        case n of
            SRNumber x -> return x
            _ -> Unknown

instance AsLiteral SafeRational where
    toLiteral = toLiteral . safeRationalToNumber
    fromLiteral t = do
        n <- fromLiteral t
        case checkExactSafeRational n of
            Just x -> return x
            _ -> Unknown

instance AsLiteral Double where
    toLiteral = toLiteral . InexactNumber
    fromLiteral t = do
        n <- fromLiteral t
        case n of
            InexactNumber x -> return x
            _ -> Unknown

instance AsLiteral Day where
    toLiteral = MkLiteral . pack . show
    fromLiteral = readFromLiteral

instance AsLiteral TimeOfDay where
    toLiteral = MkLiteral . pack . show
    fromLiteral = readFromLiteral

instance AsLiteral LocalTime where
    toLiteral = MkLiteral . pack . show
    fromLiteral = readFromLiteral

instance AsLiteral UTCTime where
    toLiteral = MkLiteral . pack . show
    fromLiteral = readFromLiteral

nominalDiffTimeToSeconds :: NominalDiffTime -> Pico
nominalDiffTimeToSeconds = realToFrac

secondsToNominalDiffTime :: Pico -> NominalDiffTime
secondsToNominalDiffTime = realToFrac

instance AsLiteral NominalDiffTime where
    toLiteral = toLiteral . nominalDiffTimeToSeconds
    fromLiteral = fmap secondsToNominalDiffTime . fromLiteral
