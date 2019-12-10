{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Value.Instances where

import Data.Shim
import Pinafore.Base
import Shapes

instance HasVariance 'Covariance Know where
    varianceRepresentational = Just Dict

instance HasVariance 'Covariance (PinaforeAction baseupdate) where
    varianceRepresentational = Just Dict
