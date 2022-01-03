module Data.Shim.PolarVariance where

import Data.Shim.CCRVariance
import Data.Shim.CatRange
import Data.Shim.JoinMeet
import Data.Shim.PolarMap
import Data.Shim.Polarity
import Data.Shim.PolyShim
import Shapes

type PolarVarianceMap (shim :: ShimKind Type) (polarity :: Polarity) (sv :: CCRVariance)
     = CCRVarianceCategory (PolarMap shim polarity) sv

mkContravariantPolarMap ::
       forall (shim :: ShimKind Type) polarity a b. Is PolarityType polarity
    => PolarMap shim (InvertPolarity polarity) a b
    -> PolarVarianceMap shim polarity ContraCCRVariance a b
mkContravariantPolarMap f = MkCatDual $ uninvertPolarMap f

mkRangevariantPolarMap ::
       forall (shim :: ShimKind Type) polarity p1 p2 q1 q2. Is PolarityType polarity
    => PolarMap shim (InvertPolarity polarity) p1 p2
    -> PolarMap shim polarity q1 q2
    -> PolarVarianceMap shim polarity 'RangeCCRVariance '( p1, q1) '( p2, q2)
mkRangevariantPolarMap pp qq = MkCatRange (uninvertPolarMap pp) qq

polarMapTypeApply ::
       forall (pmap :: PolyShimKind) (polarity :: Polarity) (v :: CCRVariance) k (f :: CCRVarianceKind v -> k) (g :: CCRVarianceKind v -> k) (a :: CCRVarianceKind v) (b :: CCRVarianceKind v).
       (ApplyPolyShim pmap, Is PolarityType polarity)
    => CCRVarianceType v
    -> CCRVariation v f
    -> CCRVariation v g
    -> PolarMap (pmap (CCRVarianceKind v -> k)) polarity f g
    -> PolarVarianceMap (pmap Type) polarity v a b
    -> PolarMap (pmap k) polarity (f a) (g b)
polarMapTypeApply CoCCRVarianceType ccrvf ccrvg (MkPolarMap fg) (MkPolarMap ab) =
    MkPolarMap $
    case polarityType @polarity of
        PositiveType -> applyPolyShim @pmap CoCCRVarianceType ccrvf ccrvg fg ab
        NegativeType -> applyPolyShim @pmap CoCCRVarianceType ccrvg ccrvf fg ab
polarMapTypeApply ContraCCRVarianceType ccrvf ccrvg (MkPolarMap fg) (MkCatDual (MkPolarMap ab)) =
    MkPolarMap $
    case polarityType @polarity of
        PositiveType -> applyPolyShim @pmap ContraCCRVarianceType ccrvf ccrvg fg (MkCatDual ab)
        NegativeType -> applyPolyShim @pmap ContraCCRVarianceType ccrvg ccrvf fg (MkCatDual ab)
polarMapTypeApply RangeCCRVarianceType ccrvf ccrvg (MkPolarMap fg) (MkCatRange (MkPolarMap abp) (MkPolarMap abq)) =
    MkPolarMap $
    case polarityType @polarity of
        PositiveType -> applyPolyShim @pmap RangeCCRVarianceType ccrvf ccrvg fg (MkCatRange abp abq)
        NegativeType -> applyPolyShim @pmap RangeCCRVarianceType ccrvg ccrvf fg (MkCatRange abp abq)
