module Data.Shim.CCR.Apply where

import Shapes

import Data.Shim.CCR.Variance
import Data.Shim.Mono
import Data.Shim.Poly
import Data.Shim.Range

type ApplyPolyShim :: PolyShimKind -> Constraint
class AllCategory pshim => ApplyPolyShim pshim where
    applyPolyShim ::
        forall k (v :: CCRVariance) (f :: CCRVarianceKind v -> k) (g :: CCRVarianceKind v -> k) (a :: CCRVarianceKind v) (b :: CCRVarianceKind v).
        CCRVarianceType v ->
        CCRVariation v f ->
        CCRVariation v g ->
        pshim (CCRVarianceKind v -> k) f g ->
        CCRVarianceCategory (pshim Type) v a b ->
        pshim k (f a) (g b)

applyCoPolyShim ::
    forall (pshim :: PolyShimKind) k (f :: Type -> k) (g :: Type -> k) (a :: Type) (b :: Type).
    ApplyPolyShim pshim =>
    CCRVariation CoCCRVariance f ->
    CCRVariation CoCCRVariance g ->
    pshim (Type -> k) f g ->
    pshim Type a b ->
    pshim k (f a) (g b)
applyCoPolyShim ccrvf ccrvg fg ab = applyPolyShim CoCCRVarianceType ccrvf ccrvg fg ab

applyContraPolyShim ::
    forall (pshim :: PolyShimKind) k (f :: Type -> k) (g :: Type -> k) (a :: Type) (b :: Type).
    ApplyPolyShim pshim =>
    CCRVariation ContraCCRVariance f ->
    CCRVariation ContraCCRVariance g ->
    pshim (Type -> k) f g ->
    pshim Type b a ->
    pshim k (f a) (g b)
applyContraPolyShim ccrvf ccrvg fg ba = applyPolyShim ContraCCRVarianceType ccrvf ccrvg fg (MkCatDual ba)

applyRangePolyShim ::
    forall (pshim :: PolyShimKind) k (f :: (Type, Type) -> k) (g :: (Type, Type) -> k) (a1 :: Type) (a2 :: Type) (b1 :: Type) (b2 :: Type).
    ApplyPolyShim pshim =>
    CCRVariation 'RangeCCRVariance f ->
    CCRVariation 'RangeCCRVariance g ->
    pshim ((Type, Type) -> k) f g ->
    pshim Type b1 a1 ->
    pshim Type a2 b2 ->
    pshim k (f '(a1, a2)) (g '(b1, b2))
applyRangePolyShim ccrvf ccrvg fg ba1 ab2 = applyPolyShim RangeCCRVarianceType ccrvf ccrvg fg (MkCatRange ba1 ab2)

instance ApplyPolyShim NullPolyShim where
    applyPolyShim CoCCRVarianceType _ _ MkNullPolyShim MkNullPolyShim = MkNullPolyShim
    applyPolyShim ContraCCRVarianceType _ _ MkNullPolyShim (MkCatDual MkNullPolyShim) = MkNullPolyShim
    applyPolyShim RangeCCRVarianceType _ _ MkNullPolyShim (MkCatRange MkNullPolyShim MkNullPolyShim) = MkNullPolyShim

instance ApplyPolyShim IdentityPolyShim where
    applyPolyShim CoCCRVarianceType _ _ MkIdentityPolyShim MkIdentityPolyShim = MkIdentityPolyShim
    applyPolyShim ContraCCRVarianceType _ _ MkIdentityPolyShim (MkCatDual MkIdentityPolyShim) = MkIdentityPolyShim
    applyPolyShim RangeCCRVarianceType _ _ MkIdentityPolyShim (MkCatRange MkIdentityPolyShim MkIdentityPolyShim) = MkIdentityPolyShim

instance forall (pshim :: PolyShimKind). ApplyPolyShim pshim => ApplyPolyShim (DualPolyT pshim) where
    applyPolyShim CoCCRVarianceType ccrvf ccrvg (MkMapPolyT (MkCatDual fba)) (MkMapPolyT (MkCatDual xba)) =
        MkMapPolyT $ MkCatDual $ applyCoPolyShim ccrvg ccrvf fba xba
    applyPolyShim ContraCCRVarianceType ccrvf ccrvg (MkMapPolyT (MkCatDual fba)) (MkCatDual (MkMapPolyT (MkCatDual xba))) =
        MkMapPolyT $ MkCatDual $ applyContraPolyShim ccrvg ccrvf fba xba
    applyPolyShim RangeCCRVarianceType ccrvf ccrvg (MkMapPolyT (MkCatDual fba)) (MkCatRange (MkMapPolyT (MkCatDual xba1)) (MkMapPolyT (MkCatDual xba2))) =
        MkMapPolyT $ MkCatDual $ applyRangePolyShim ccrvg ccrvf fba xba1 xba2

instance forall (pshim :: PolyShimKind). ApplyPolyShim pshim => ApplyPolyShim (IsoPolyT pshim) where
    applyPolyShim CoCCRVarianceType ccrvf ccrvg (MkMapPolyT (MkIsomorphism fab fba)) (MkMapPolyT (MkIsomorphism xab xba)) =
        MkMapPolyT $ MkIsomorphism (applyCoPolyShim ccrvf ccrvg fab xab) (applyCoPolyShim ccrvg ccrvf fba xba)
    applyPolyShim ContraCCRVarianceType ccrvf ccrvg (MkMapPolyT (MkIsomorphism fab fba)) (MkCatDual (MkMapPolyT (MkIsomorphism xab xba))) =
        MkMapPolyT $ MkIsomorphism (applyContraPolyShim ccrvf ccrvg fab xab) (applyContraPolyShim ccrvg ccrvf fba xba)
    applyPolyShim RangeCCRVarianceType ccrvf ccrvg (MkMapPolyT (MkIsomorphism fab fba)) (MkCatRange (MkMapPolyT (MkIsomorphism xab1 xba1)) (MkMapPolyT (MkIsomorphism xab2 xba2))) =
        MkMapPolyT
            $ MkIsomorphism (applyRangePolyShim ccrvf ccrvg fab xab1 xab2) (applyRangePolyShim ccrvg ccrvf fba xba1 xba2)

instance
    forall (pshim :: PolyShimKind) m.
    (ApplyPolyShim pshim, Applicative m) =>
    ApplyPolyShim (ComposePolyT m pshim)
    where
    applyPolyShim CoCCRVarianceType ccrvf ccrvg (MkMapPolyT (MkComposeShim mfab)) (MkMapPolyT (MkComposeShim mxab)) =
        MkMapPolyT $ MkComposeShim (liftA2 (applyCoPolyShim ccrvf ccrvg) mfab mxab)
    applyPolyShim ContraCCRVarianceType ccrvf ccrvg (MkMapPolyT (MkComposeShim mfab)) (MkCatDual (MkMapPolyT (MkComposeShim mxab))) =
        MkMapPolyT $ MkComposeShim ((\fab xab -> applyContraPolyShim ccrvf ccrvg fab xab) <$> mfab <*> mxab)
    applyPolyShim RangeCCRVarianceType ccrvf ccrvg (MkMapPolyT (MkComposeShim mfab)) (MkCatRange (MkMapPolyT (MkComposeShim mxab1)) (MkMapPolyT (MkComposeShim mxab2))) =
        MkMapPolyT
            $ MkComposeShim ((\fab xab1 xab2 -> applyRangePolyShim ccrvf ccrvg fab xab1 xab2) <$> mfab <*> mxab1 <*> mxab2)
