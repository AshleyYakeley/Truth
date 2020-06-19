module Data.Shim.PolarVariance where

import Data.Shim.CatRange
import Data.Shim.PolarMap
import Data.Shim.Polarity
import Data.Shim.PolyMap
import Data.Shim.PolyShim
import Data.Shim.Variance
import Shapes

type PolarVarianceMap (shim :: ShimKind Type) (polarity :: Polarity) (sv :: Variance)
     = VarianceCategory (PolarMap shim polarity) sv

mkContravariantPolarMap ::
       forall (shim :: ShimKind Type) polarity a b. Is PolarityType polarity
    => PolarMap shim (InvertPolarity polarity) a b
    -> PolarVarianceMap shim polarity 'Contravariance a b
mkContravariantPolarMap f = MkCatDual $ uninvertPolarMap f

mkRangevariantPolarMap ::
       forall (shim :: ShimKind Type) polarity p1 p2 q1 q2. Is PolarityType polarity
    => PolarMap shim (InvertPolarity polarity) p1 p2
    -> PolarMap shim polarity q1 q2
    -> PolarVarianceMap shim polarity 'Rangevariance '( p1, q1) '( p2, q2)
mkRangevariantPolarMap pp qq = MkCatRange (uninvertPolarMap pp) qq

polarMapTypeApply ::
       forall (pmap :: PolyShimKind) (polarity :: Polarity) (v :: Variance) k (f :: VarianceKind v -> k) (g :: VarianceKind v -> k) (a :: VarianceKind v) (b :: VarianceKind v).
       (ApplyPolyShim pmap, Is PolarityType polarity, InKind a, InKind b, HasVariance v f, HasVariance v g)
    => VarianceType v
    -> PolarMap (pmap (VarianceKind v -> k)) polarity f g
    -> PolarVarianceMap (pmap Type) polarity v a b
    -> PolarMap (pmap k) polarity (f a) (g b)
polarMapTypeApply CovarianceType (MkPolarMap fg) (MkPolarMap ab) =
    MkPolarMap $
    case polarityType @polarity of
        PositiveType -> applyPolyShim @pmap CovarianceType fg ab
        NegativeType -> applyPolyShim @pmap CovarianceType fg ab
polarMapTypeApply ContravarianceType (MkPolarMap fg) (MkCatDual (MkPolarMap ab)) =
    MkPolarMap $
    case polarityType @polarity of
        PositiveType -> applyPolyShim @pmap ContravarianceType fg (MkCatDual ab)
        NegativeType -> applyPolyShim @pmap ContravarianceType fg (MkCatDual ab)
polarMapTypeApply RangevarianceType (MkPolarMap fg) (MkCatRange (MkPolarMap abp) (MkPolarMap abq)) =
    MkPolarMap $
    case polarityType @polarity of
        PositiveType -> applyPolyShim @pmap RangevarianceType fg (MkCatRange abp abq)
        NegativeType -> applyPolyShim @pmap RangevarianceType fg (MkCatRange abp abq)
