module Pinafore.Base.Text.PrecText
    ( PrecText
    , textPrec
    , textToPrec
    , precText
    )
where

import Shapes

import Pinafore.Base.Text.ToText

data PrecText
    = MkPrecText Int Text

instance Semigroup PrecText where
    MkPrecText pa ta <> MkPrecText pb tb = MkPrecText (max pa pb) (ta <> tb)

instance Monoid PrecText where
    mempty = MkPrecText 0 mempty

textPrec :: Int -> Text -> PrecText
textPrec = MkPrecText

textToPrec :: Text -> PrecText
textToPrec = textPrec 0

instance IsString PrecText where
    fromString s = textToPrec $ pack s

precText :: Int -> PrecText -> Text
precText c (MkPrecText p t) =
    if c < p
        then "(" <> t <> ")"
        else t

instance ToText PrecText where
    toText (MkPrecText _ t) = t
