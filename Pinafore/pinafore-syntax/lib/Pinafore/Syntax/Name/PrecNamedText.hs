module Pinafore.Syntax.Name.PrecNamedText where

import Pinafore.Syntax.Name.NamedText
import Shapes

newtype PrecNamedText =
    MkPrecNamedText ((NamedTextItem -> Text) -> (Text, Int))

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
