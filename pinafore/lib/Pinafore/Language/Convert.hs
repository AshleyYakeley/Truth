{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Convert
    ( module Pinafore.Language.Convert.Base
    ) where

import Data.Ratio
import Pinafore.Know
import Pinafore.Language.Convert.Base
import Pinafore.Language.Entity
import Pinafore.Language.Type
import Pinafore.Literal
import Pinafore.Number
import Shapes
import Truth.Core

-- UIWindow
instance baseedit ~ edit => FromTypeF (PinaforeType baseedit) (UIWindow edit) where
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
instance ToTypeF (PinaforeType baseedit) Double where
    toTypeF = contramap InexactNumber toTypeF

-- Rational
instance ToTypeF (PinaforeType baseedit) Rational where
    toTypeF = contramap ExactNumber toTypeF

-- Integer
instance ToTypeF (PinaforeType baseedit) Integer where
    toTypeF = contramap (ExactNumber . toRational) toTypeF

-- Int
instance ToTypeF (PinaforeType baseedit) Int where
    toTypeF = contramap (ExactNumber . toRational) toTypeF
