{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Value.Instances where

import Import

instance MaybeRepresentational Equivalence where
    maybeRepresentational = Just Dict

instance HasVariance Equivalence where
    type VarianceOf Equivalence = 'Contravariance

instance MaybeRepresentational Preorder where
    maybeRepresentational = Just Dict

instance HasVariance Preorder where
    type VarianceOf Preorder = 'Contravariance

instance MaybeRepresentational Order where
    maybeRepresentational = Just Dict

instance HasVariance Order where
    type VarianceOf Order = 'Contravariance

instance MaybeRepresentational Know where
    maybeRepresentational = Just Dict

instance HasVariance Know where
    type VarianceOf Know = 'Covariance

instance MaybeRepresentational Action where
    maybeRepresentational = Just Dict

instance HasVariance Action where
    type VarianceOf Action = 'Covariance
