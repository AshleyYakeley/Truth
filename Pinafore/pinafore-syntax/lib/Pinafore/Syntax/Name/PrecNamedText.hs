module Pinafore.Syntax.Name.PrecNamedText
    ( PrecNamedText
    , namedTextPrec
    , namedTextToPrec
    , identifierPrecNamedText
    , precNamedText
    , applyPrecNamedText
    , applyOpPrecNamedText
    )
where

import Pinafore.Base
import Shapes

import Pinafore.Syntax.Name.NamedText

newtype PrecNamedText
    = MkPrecNamedText ((NamedTextItem -> Text) -> PrecText)
    deriving newtype (Semigroup, Monoid)

namedTextPrec :: Word -> NamedText -> PrecNamedText
namedTextPrec c (MkNamedText ftt) = MkPrecNamedText $ \ft -> textPrec c $ ftt ft

namedTextToPrec :: NamedText -> PrecNamedText
namedTextToPrec = namedTextPrec 0

identifierPrecNamedText :: Text -> PrecNamedText
identifierPrecNamedText t = namedTextToPrec $ toNamedText t

instance IsString PrecNamedText where
    fromString s = identifierPrecNamedText $ pack s

precNamedText :: Word -> PrecNamedText -> NamedText
precNamedText c (MkPrecNamedText pnt) =
    MkNamedText $ \fnt -> precText c $ pnt fnt

instance ToNamedText PrecNamedText where
    toNamedText (MkPrecNamedText pnt) = MkNamedText $ \ft -> toText $ pnt ft

applyPrecNamedText :: PrecNamedText -> PrecNamedText -> PrecNamedText
applyPrecNamedText (MkPrecNamedText f) (MkPrecNamedText x) = MkPrecNamedText $ \ntt -> applyPrecText (f ntt) (x ntt)

applyOpPrecNamedText :: PrecNamedText -> (Text, Fixity) -> PrecNamedText -> PrecNamedText
applyOpPrecNamedText (MkPrecNamedText a) opc (MkPrecNamedText b) = MkPrecNamedText $ \ntt -> applyOpPrecText (a ntt) opc (b ntt)
