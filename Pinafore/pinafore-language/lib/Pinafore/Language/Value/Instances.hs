{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Value.Instances where

import Data.Shim
import Pinafore.Base
import Shapes

instance MaybeRepresentational Know where
    maybeRepresentational = Just Dict

instance HasVariance Know where
    type VarianceOf Know = 'Covariance

instance MaybeRepresentational Action where
    maybeRepresentational = Just Dict

instance HasVariance Action where
    type VarianceOf Action = 'Covariance
