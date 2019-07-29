{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Convert
    ( module Pinafore.Language.Convert.Base
    ) where

import Data.Time
import Pinafore.Base
import Pinafore.Language.Convert.Base
import Pinafore.Language.Type
import Shapes
import Shapes.Numeric

-- Literal types
$(literalInstances [t|Literal|])

$(literalInstances [t|Text|])

$(literalInstances [t|Number|])

$(literalInstances [t|Rational|])

$(literalInstances [t|Integer|])

$(literalInstances [t|Bool|])

$(literalInstances [t|UTCTime|])

$(literalInstances [t|NominalDiffTime|])

$(literalInstances [t|Day|])

$(literalInstances [t|TimeOfDay|])

$(literalInstances [t|LocalTime|])

$(literalInstances [t|()|])

-- Double
instance ToShimWit JMShim (PinaforeType baseedit 'Positive) Double where
    toShimWit = mapShimWit (toEnhanced InexactNumber) toJMShimWit

-- Int
instance ToShimWit JMShim (PinaforeType baseedit 'Positive) Int where
    toShimWit = mapShimWit (toEnhanced toInteger) toJMShimWit

instance FromShimWit JMShim (PinaforeType baseedit 'Negative) Int where
    fromShimWit = mapShimWit (toEnhanced fromInteger) fromJMShimWit

-- Fixed
instance HasResolution r => ToShimWit JMShim (PinaforeType baseedit 'Positive) (Fixed r) where
    toShimWit = mapShimWit (toEnhanced toRational) toJMShimWit

instance HasResolution r => FromShimWit JMShim (PinaforeType baseedit 'Negative) (Fixed r) where
    fromShimWit = mapShimWit (toEnhanced fromRational) fromJMShimWit
