{-# OPTIONS -fno-warn-orphans #-}
module Pinafore.Syntax.ReadShow where

import Pinafore.Base
import Shapes
import Shapes.Numeric

-- This is only for "lifting" values to Pinafore expressions.
class ShowPinafore a where
    showPinafore :: a -> PrecText

instance ShowPinafore Void where
    showPinafore = never

instance ShowPinafore Bool where
    showPinafore = nameTextToPrec . showT

instance ShowPinafore Ordering where
    showPinafore = nameTextToPrec . showT

instance ShowPinafore Int where
    showPinafore = nameTextToPrec . showT

instance ShowPinafore Natural where
    showPinafore = nameTextToPrec . showT

instance ShowPinafore Integer where
    showPinafore = nameTextToPrec . showT

instance ShowPinafore SafeRational where
    showPinafore = nameTextToPrec . showImproperSafeRational

instance ShowPinafore Double where
    showPinafore d = nameTextToPrec $ "~" <> showT d

instance ShowPinafore Number where
    showPinafore (ExactNumber r) = showPinafore (SRNumber r)
    showPinafore (InexactNumber d)
        | isNaN d = showPinafore SRNaN
    showPinafore (InexactNumber d) = showPinafore d

instance Show Number where
    show = unpack . toText . showPinafore

instance ShowPinafore Text where
    showPinafore = nameTextToPrec . showT

instance (ShowPinafore a, ShowPinafore b) => ShowPinafore (Either a b) where
    showPinafore = \case
        Left x -> applyPrecText "Left" $ showPinafore x
        Right x -> applyPrecText "Right" $ showPinafore x
