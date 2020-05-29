module Data.Shim.PolarVariance where

import Data.Shim.CatRange
import Data.Shim.PolarMap
import Data.Shim.Polarity
import Data.Shim.PolyShim
import Data.Shim.Variance
import Shapes

type PolarVarianceMap (cat :: Type -> Type -> Type) (polarity :: Polarity) (sv :: Variance)
     = VarianceCategory (PolarMap cat polarity) sv

mkContravariantPolarMap ::
       forall cat polarity a b. Is PolarityType polarity
    => PolarMap cat (InvertPolarity polarity) a b
    -> PolarVarianceMap cat polarity 'Contravariance a b
mkContravariantPolarMap f = MkCatDual $ uninvertPolarMap f

mkRangevariantPolarMap ::
       forall cat polarity p1 p2 q1 q2. Is PolarityType polarity
    => PolarMap cat (InvertPolarity polarity) p1 p2
    -> PolarMap cat polarity q1 q2
    -> PolarVarianceMap cat polarity 'Rangevariance '( p1, q1) '( p2, q2)
mkRangevariantPolarMap pp qq = MkCatRange (uninvertPolarMap pp) qq

consPolarVarianceMap ::
       forall (shim :: forall kc. kc -> kc -> Type) (polarity :: Polarity) (v :: Variance) k (f :: VarianceKind v -> k) (g :: VarianceKind v -> k) (a :: VarianceKind v) (b :: VarianceKind v).
       (ConPolyShim shim, Is PolarityType polarity, InKind a, InKind b, HasVariance v f, HasVariance v g)
    => VarianceType v
    -> PolarMap shim polarity f g
    -> PolarVarianceMap shim polarity v a b
    -> PolarMap shim polarity (f a) (g b)
-- FIXME: requires GHC 8.10, use kind "forall k -> k -> k -> Type"
--consPolarVarianceMap vt = consShimFunc @(PolarMap shim polarity) vt
consPolarVarianceMap CovarianceType (MkPolarMap fg) (MkPolarMap ab) =
    MkPolarMap $
    case polarityType @polarity of
        PositiveType -> consShimFunc @shim CovarianceType fg ab
        NegativeType -> consShimFunc @shim CovarianceType fg ab
consPolarVarianceMap ContravarianceType (MkPolarMap fg) (MkCatDual (MkPolarMap ab)) =
    MkPolarMap $
    case polarityType @polarity of
        PositiveType -> consShimFunc @shim ContravarianceType fg (MkCatDual ab)
        NegativeType -> consShimFunc @shim ContravarianceType fg (MkCatDual ab)
consPolarVarianceMap RangevarianceType (MkPolarMap fg) (MkCatRange (MkPolarMap abp) (MkPolarMap abq)) =
    MkPolarMap $
    case polarityType @polarity of
        PositiveType -> consShimFunc @shim RangevarianceType fg (MkCatRange abp abq)
        NegativeType -> consShimFunc @shim RangevarianceType fg (MkCatRange abp abq)
