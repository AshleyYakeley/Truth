{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Convert
    ( module Pinafore.Language.Convert.Base
    ) where

import Data.Shim
import Data.Time
import Pinafore.Base
import Pinafore.Language.Convert.Base
import Pinafore.Language.Shim
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
instance ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) Double where
    toShimWit = mapPosShimWit (toEnhanced "subtype" InexactNumber) toJMShimWit

-- Int
instance ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) Int where
    toShimWit = mapPosShimWit (toEnhanced "subtype" toInteger) toJMShimWit

instance FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) Int where
    fromShimWit = mapNegShimWit (toEnhanced "subtype" fromInteger) fromJMShimWit

-- Rational
instance ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) Rational where
    toShimWit = mapPosShimWit (toEnhanced "subtype" $ fromRational @SafeRational) toJMShimWit

instance FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) Rational where
    fromShimWit = mapNegShimWit (toEnhanced "subtype" $ toRational @SafeRational) fromJMShimWit

-- Fixed
instance HasResolution r => ToShimWit (PinaforeShim Type) (PinaforeType 'Positive) (Fixed r) where
    toShimWit = mapPosShimWit (toEnhanced "subtype" toRational) toJMShimWit

instance HasResolution r => FromShimWit (PinaforeShim Type) (PinaforeType 'Negative) (Fixed r) where
    fromShimWit = mapNegShimWit (toEnhanced "subtype" fromRational) fromJMShimWit
