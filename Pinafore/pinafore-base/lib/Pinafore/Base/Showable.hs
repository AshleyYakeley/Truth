module Pinafore.Base.Showable where

import Shapes

class ShowText t where
    -- | NOT the same as 'toText'.
    showText :: t -> Text
    default showText :: Show t => t -> Text
    showText v = pack $ show v

instance {-# OVERLAPPABLE #-} Show t => ShowText t

instance (ShowText a, ShowText b) => ShowText (Result a b) where
    showText (FailureResult a) = "Failure " <> showText a
    showText (SuccessResult b) = "Success " <> showText b

newtype Showable
    = MkShowable Text

instance ShowText Showable where
    showText (MkShowable t) = t

instance Show Showable where
    show v = unpack $ showText v

textShowable :: ShowText t => t -> Showable
textShowable v = MkShowable $ showText v
