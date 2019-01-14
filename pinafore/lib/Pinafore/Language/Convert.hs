{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Convert
    ( module Pinafore.Language.Convert.Base
    ) where

import Data.Ratio
import Pinafore.Base
import Pinafore.Language.Convert.Base
import Pinafore.Language.Type
import Shapes
import Truth.Core

-- UIWindow
instance baseedit ~ edit => FromTypeF (PinaforeType baseedit 'Negative) (UIWindow edit) where
    fromTypeF =
        fmap
            (\(title, content :: UISpec (ConstEdit Entity) edit) ->
                 MkUIWindow (funcEditFunction @(WholeEdit (Know Text)) (fromKnow "") . title) content)
            fromTypeF

-- Literal types
$(literalInstances [t|Literal|])

$(literalInstances [t|Text|])

$(literalInstances [t|Number|])

$(literalInstances [t|Bool|])

$(literalInstances [t|()|])

-- Double
instance ToTypeF (PinaforeType baseedit 'Positive) Double where
    toTypeF = contramap InexactNumber toTypeF

-- Rational
instance ToTypeF (PinaforeType baseedit 'Positive) Rational where
    toTypeF = contramap ExactNumber toTypeF

-- Integer
instance ToTypeF (PinaforeType baseedit 'Positive) Integer where
    toTypeF = contramap (ExactNumber . toRational) toTypeF

-- Int
instance ToTypeF (PinaforeType baseedit 'Positive) Int where
    toTypeF = contramap (ExactNumber . toRational) toTypeF
