module Pinafore.Syntax.Name.PrecNamedText where

import Shapes

import Pinafore.Syntax.Name.NamedText

newtype PrecNamedText
    = MkPrecNamedText ((NamedTextItem -> Text) -> (Text, Int))

instance Semigroup PrecNamedText where
    MkPrecNamedText ftta <> MkPrecNamedText fttb = MkPrecNamedText $ \ft ->
        let
            (ta, pa) = ftta ft
            (tb, pb) = fttb ft
            in (ta <> tb, max pa pb)

instance Monoid PrecNamedText where
    mempty = MkPrecNamedText $ \_ -> (mempty, 0)

namedTextPrec :: Int -> NamedText -> PrecNamedText
namedTextPrec c (MkNamedText ftt) = MkPrecNamedText $ \ft -> (ftt ft, c)

namedTextToPrec :: NamedText -> PrecNamedText
namedTextToPrec = namedTextPrec 0

identifierPrecNamedText :: Text -> PrecNamedText
identifierPrecNamedText t = namedTextToPrec $ toNamedText t

instance IsString PrecNamedText where
    fromString s = identifierPrecNamedText $ pack s

precNamedText :: Int -> PrecNamedText -> NamedText
precNamedText c (MkPrecNamedText pnt) =
    MkNamedText $ \fnt -> let
        (s, p) = pnt fnt
        in if c < p
            then "(" <> s <> ")"
            else s

instance ToNamedText PrecNamedText where
    toNamedText (MkPrecNamedText pnt) = MkNamedText $ \ft -> fst $ pnt ft
