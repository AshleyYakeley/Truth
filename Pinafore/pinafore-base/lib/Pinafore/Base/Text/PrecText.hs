module Pinafore.Base.Text.PrecText
    ( PrecText
    , textPrec
    , nameTextToPrec
    , precText
    , applyPrecText
    , FixAssoc (..)
    , Fixity (..)
    , applyOpPrecText
    )
where

import Shapes

import Pinafore.Base.Text.ToText

data PrecText
    = MkPrecText Word Text

instance Semigroup PrecText where
    MkPrecText pa ta <> MkPrecText pb tb = MkPrecText (max pa pb) (ta <> tb)

instance Monoid PrecText where
    mempty = MkPrecText 0 mempty

textPrec :: Word -> Text -> PrecText
textPrec = MkPrecText

nameTextToPrec :: Text -> PrecText
nameTextToPrec = textPrec 0

instance IsString PrecText where
    fromString s = nameTextToPrec $ fromString s

precText :: Word -> PrecText -> Text
precText c (MkPrecText p t) =
    if c < p
        then "(" <> t <> ")"
        else t

instance ToText PrecText where
    toText (MkPrecText _ t) = t

applyPrecText :: PrecText -> PrecText -> PrecText
applyPrecText f x = textPrec 1 $ precText 1 f <> " " <> precText 0 x

data FixAssoc
    = AssocNone
    | AssocLeft
    | AssocRight
    deriving stock Eq

data Fixity = MkFixity
    { fixityAssoc :: FixAssoc
    , fixityPrec :: Word
    }
    deriving stock Eq

applyOpPrecText :: PrecText -> (Text, Fixity) -> PrecText -> PrecText
applyOpPrecText a (op, MkFixity assc prec) b = let
    (leftPrec, rightPrec) = case assc of
        AssocNone -> (prec, prec)
        AssocLeft -> (succ prec, prec)
        AssocRight -> (prec, succ prec)
    in textPrec (succ prec) $ precText leftPrec a <> " " <> op <> " " <> precText rightPrec b
