{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Instances where

import Data.Shim
import Pinafore.Base
import Shapes

instance HasVariance 'Covariance Know where
    varianceRepresentational = Just Dict

instance HasVariance 'Covariance (PinaforeAction baseedit) where
    varianceRepresentational = Just Dict
