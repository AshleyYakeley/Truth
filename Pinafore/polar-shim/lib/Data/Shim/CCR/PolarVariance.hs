module Data.Shim.CCR.PolarVariance where

import Shapes

import Data.Shim.CCR.Apply
import Data.Shim.CCR.Variance
import Data.Shim.Mono
import Data.Shim.Polar
import Data.Shim.Poly
import Data.Shim.Range

type PolarVarianceMap (shim :: ShimKind Type) (polarity :: Polarity) (sv :: CCRVariance) =
    CCRVarianceCategory (PolarShim shim polarity) sv

mkContravariantPolarShim ::
    forall (shim :: ShimKind Type) polarity a b.
    Is PolarityType polarity =>
    PolarShim shim (InvertPolarity polarity) a b ->
    PolarVarianceMap shim polarity ContraCCRVariance a b
mkContravariantPolarShim f = MkCatDual $ uninvertPolarShim f

mkRangevariantPolarShim ::
    forall (shim :: ShimKind Type) polarity p1 p2 q1 q2.
    Is PolarityType polarity =>
    PolarShim shim (InvertPolarity polarity) p1 p2 ->
    PolarShim shim polarity q1 q2 ->
    PolarVarianceMap shim polarity 'RangeCCRVariance '(p1, q1) '(p2, q2)
mkRangevariantPolarShim pp qq = MkCatRange (uninvertPolarShim pp) qq

polarShimTypeApply ::
    forall (pmap :: PolyShimKind) (polarity :: Polarity) (v :: CCRVariance) k (f :: CCRVarianceKind v -> k) (g :: CCRVarianceKind v -> k) (a :: CCRVarianceKind v) (b :: CCRVarianceKind v).
    (ApplyPolyShim pmap, Is PolarityType polarity) =>
    CCRVarianceType v ->
    CCRVariation v f ->
    CCRVariation v g ->
    PolarShim (pmap (CCRVarianceKind v -> k)) polarity f g ->
    PolarVarianceMap (pmap Type) polarity v a b ->
    PolarShim (pmap k) polarity (f a) (g b)
polarShimTypeApply CoCCRVarianceType ccrvf ccrvg (MkPolarShim fg) (MkPolarShim ab) =
    MkPolarShim
        $ case polarityType @polarity of
            PositiveType -> applyPolyShim @pmap CoCCRVarianceType ccrvf ccrvg fg ab
            NegativeType -> applyPolyShim @pmap CoCCRVarianceType ccrvg ccrvf fg ab
polarShimTypeApply ContraCCRVarianceType ccrvf ccrvg (MkPolarShim fg) (MkCatDual (MkPolarShim ab)) =
    MkPolarShim
        $ case polarityType @polarity of
            PositiveType -> applyPolyShim @pmap ContraCCRVarianceType ccrvf ccrvg fg (MkCatDual ab)
            NegativeType -> applyPolyShim @pmap ContraCCRVarianceType ccrvg ccrvf fg (MkCatDual ab)
polarShimTypeApply RangeCCRVarianceType ccrvf ccrvg (MkPolarShim fg) (MkCatRange (MkPolarShim abp) (MkPolarShim abq)) =
    MkPolarShim
        $ case polarityType @polarity of
            PositiveType -> applyPolyShim @pmap RangeCCRVarianceType ccrvf ccrvg fg (MkCatRange abp abq)
            NegativeType -> applyPolyShim @pmap RangeCCRVarianceType ccrvg ccrvf fg (MkCatRange abp abq)
