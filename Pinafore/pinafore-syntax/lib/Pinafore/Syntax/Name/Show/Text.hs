module Pinafore.Syntax.Name.Show.Text where

import Pinafore.Base
import Shapes

import Pinafore.Syntax.ReadShow

class ShowText t where
    -- | NOT the same as 'toText'.
    showText :: t -> Text
    default showText :: Show t => t -> Text
    showText = showT

instance {-# OVERLAPPABLE #-} Show t => ShowText t

instance (ShowText a, ShowText b) => ShowText (Result a b) where
    showText (FailureResult a) = "Failure " <> showText a
    showText (SuccessResult b) = "Success " <> showText b

instance ShowText Int where
    showText = toText . showPinafore

instance ShowText Natural where
    showText = toText . showPinafore

instance ShowText Integer where
    showText = toText . showPinafore

instance ShowText SafeRational where
    showText = toText . showPinafore

instance ShowText Number where
    showText = toText . showPinafore

instance ShowText Text where
    showText = toText . showPinafore

instance ShowText (SymbolType symbol) where
    showText = pack . witnessToValue
