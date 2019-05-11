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
instance ToTypeF (PinaforeType baseedit 'Positive) Double where
    toTypeF = contramap InexactNumber toTypeF

-- Int
instance ToTypeF (PinaforeType baseedit 'Positive) Int where
    toTypeF = contramap toInteger toTypeF

instance FromTypeF (PinaforeType baseedit 'Negative) Int where
    fromTypeF = fmap fromInteger fromTypeF

-- Fixed
instance HasResolution r => ToTypeF (PinaforeType baseedit 'Positive) (Fixed r) where
    toTypeF = contramap toRational toTypeF

instance HasResolution r => FromTypeF (PinaforeType baseedit 'Negative) (Fixed r) where
    fromTypeF = fmap fromRational fromTypeF
