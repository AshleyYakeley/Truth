module Pinafore.Syntax.Name.PrecNamedText
    ( PrecNamedText
    , namedTextPrec
    , namedTextToPrec
    , identifierPrecNamedText
    , precNamedText
    )
where

import Pinafore.Base
import Shapes

import Pinafore.Syntax.Name.NamedText

newtype PrecNamedText
    = MkPrecNamedText ((NamedTextItem -> Text) -> PrecText)
    deriving newtype (Semigroup, Monoid)

namedTextPrec :: Int -> NamedText -> PrecNamedText
namedTextPrec c (MkNamedText ftt) = MkPrecNamedText $ \ft -> textPrec c $ ftt ft

namedTextToPrec :: NamedText -> PrecNamedText
namedTextToPrec = namedTextPrec 0

identifierPrecNamedText :: Text -> PrecNamedText
identifierPrecNamedText t = namedTextToPrec $ toNamedText t

instance IsString PrecNamedText where
    fromString s = identifierPrecNamedText $ pack s

precNamedText :: Int -> PrecNamedText -> NamedText
precNamedText c (MkPrecNamedText pnt) =
    MkNamedText $ \fnt -> precText c $ pnt fnt

instance ToNamedText PrecNamedText where
    toNamedText (MkPrecNamedText pnt) = MkNamedText $ \ft -> toText $ pnt ft
