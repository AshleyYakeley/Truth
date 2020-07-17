{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Convert
    ( module Pinafore.Language.Convert.Base
    ) where

import Data.Time
import Pinafore.Base
import Pinafore.Language.Convert.Base
import Pinafore.Language.Shim
import Pinafore.Language.Type
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
instance ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) Double where
    toShimWit = mapPosShimWit (functionToShim "subtype" InexactNumber) toJMShimWit

-- Int
instance ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) Int where
    toShimWit = mapPosShimWit (functionToShim "subtype" toInteger) toJMShimWit

instance FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) Int where
    fromShimWit = mapNegShimWit (functionToShim "subtype" fromInteger) fromJMShimWit

-- Rational
instance ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) Rational where
    toShimWit = mapPosShimWit (functionToShim "subtype" $ fromRational @SafeRational) toJMShimWit

instance FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) Rational where
    fromShimWit = mapNegShimWit (functionToShim "subtype" $ toRational @SafeRational) fromJMShimWit

-- Fixed
instance HasResolution r => ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (Fixed r) where
    toShimWit = mapPosShimWit (functionToShim "subtype" toRational) toJMShimWit

instance HasResolution r => FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (Fixed r) where
    fromShimWit = mapNegShimWit (functionToShim "subtype" fromRational) fromJMShimWit
