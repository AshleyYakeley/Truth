module Pinafore.Base.Text.PrecText
    ( PrecText
    , textPrec
    , nameTextToPrec
    , precText
    , applyPrecText
    , applyOpLPrecText
    , applyOpRPrecText
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

nameTextToPrec :: Text -> PrecText
nameTextToPrec = textPrec 0

instance IsString PrecText where
    fromString s = nameTextToPrec $ pack s

precText :: Int -> PrecText -> Text
precText c (MkPrecText p t) =
    if c < p
        then "(" <> t <> ")"
        else t

instance ToText PrecText where
    toText (MkPrecText _ t) = t

applyPrecText :: PrecText -> PrecText -> PrecText
applyPrecText f x = textPrec 1 $ toText f <> " " <> precText 0 x

applyOpLPrecText :: PrecText -> (Text, Int) -> PrecText -> PrecText
applyOpLPrecText a (op, c) b = textPrec (succ c) $ precText c a <> " " <> op <> " " <> precText (succ c) b

applyOpRPrecText :: PrecText -> (Text, Int) -> PrecText -> PrecText
applyOpRPrecText a (op, c) b = textPrec (succ c) $ precText (succ c) a <> " " <> op <> " " <> precText c b
