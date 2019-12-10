{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Convert
    ( module Pinafore.Language.Convert.Base
    ) where

import Data.Shim
import Data.Time
import Pinafore.Base
import Pinafore.Language.Convert.Base
import Pinafore.Language.TypeSystem
import Shapes
import Shapes.Numeric

-- Literal types
$(literalInstances [t|Literal|])

$(literalInstances [t|Text|])

$(literalInstances [t|Number|])

$(literalInstances [t|SafeRational|])

$(literalInstances [t|Integer|])

$(literalInstances [t|Bool|])

$(literalInstances [t|UTCTime|])

$(literalInstances [t|NominalDiffTime|])

$(literalInstances [t|Day|])

$(literalInstances [t|TimeOfDay|])

$(literalInstances [t|LocalTime|])

$(literalInstances [t|()|])

-- Double
instance ToShimWit JMShim (PinaforeType baseupdate 'Positive) Double where
    toShimWit = mapShimWit (toEnhanced "subtype" InexactNumber) toJMShimWit

-- Int
instance ToShimWit JMShim (PinaforeType baseupdate 'Positive) Int where
    toShimWit = mapShimWit (toEnhanced "subtype" toInteger) toJMShimWit

instance FromShimWit JMShim (PinaforeType baseupdate 'Negative) Int where
    fromShimWit = mapShimWit (toEnhanced "subtype" fromInteger) fromJMShimWit

-- Rational
instance ToShimWit JMShim (PinaforeType baseupdate 'Positive) Rational where
    toShimWit = mapShimWit (toEnhanced "subtype" $ fromRational @SafeRational) toJMShimWit

instance FromShimWit JMShim (PinaforeType baseupdate 'Negative) Rational where
    fromShimWit = mapShimWit (toEnhanced "subtype" $ toRational @SafeRational) fromJMShimWit

-- Fixed
instance HasResolution r => ToShimWit JMShim (PinaforeType baseupdate 'Positive) (Fixed r) where
    toShimWit = mapShimWit (toEnhanced "subtype" toRational) toJMShimWit

instance HasResolution r => FromShimWit JMShim (PinaforeType baseupdate 'Negative) (Fixed r) where
    fromShimWit = mapShimWit (toEnhanced "subtype" fromRational) fromJMShimWit
