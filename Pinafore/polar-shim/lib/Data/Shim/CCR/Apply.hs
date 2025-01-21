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

instance ApplyPolyShim NullShim where
    applyPolyShim CoCCRVarianceType _ _ MkNullShim MkNullShim = MkNullShim
    applyPolyShim ContraCCRVarianceType _ _ MkNullShim (MkCatDual MkNullShim) = MkNullShim
    applyPolyShim RangeCCRVarianceType _ _ MkNullShim (MkCatRange MkNullShim MkNullShim) = MkNullShim

instance ApplyPolyShim IdentityShim where
    applyPolyShim CoCCRVarianceType _ _ MkIdentityShim MkIdentityShim = MkIdentityShim
    applyPolyShim ContraCCRVarianceType _ _ MkIdentityShim (MkCatDual MkIdentityShim) = MkIdentityShim
    applyPolyShim RangeCCRVarianceType _ _ MkIdentityShim (MkCatRange MkIdentityShim MkIdentityShim) = MkIdentityShim

instance forall (pshim :: PolyShimKind). ApplyPolyShim pshim => ApplyPolyShim (PolyDual pshim) where
    applyPolyShim CoCCRVarianceType ccrvf ccrvg (MkPolyMapT (MkCatDual fba)) (MkPolyMapT (MkCatDual xba)) =
        MkPolyMapT $ MkCatDual $ applyCoPolyShim ccrvg ccrvf fba xba
    applyPolyShim ContraCCRVarianceType ccrvf ccrvg (MkPolyMapT (MkCatDual fba)) (MkCatDual (MkPolyMapT (MkCatDual xba))) =
        MkPolyMapT $ MkCatDual $ applyContraPolyShim ccrvg ccrvf fba xba
    applyPolyShim RangeCCRVarianceType ccrvf ccrvg (MkPolyMapT (MkCatDual fba)) (MkCatRange (MkPolyMapT (MkCatDual xba1)) (MkPolyMapT (MkCatDual xba2))) =
        MkPolyMapT $ MkCatDual $ applyRangePolyShim ccrvg ccrvf fba xba1 xba2

instance forall (pshim :: PolyShimKind). ApplyPolyShim pshim => ApplyPolyShim (PolyIso pshim) where
    applyPolyShim CoCCRVarianceType ccrvf ccrvg (MkPolyMapT (MkIsomorphism fab fba)) (MkPolyMapT (MkIsomorphism xab xba)) =
        MkPolyMapT $ MkIsomorphism (applyCoPolyShim ccrvf ccrvg fab xab) (applyCoPolyShim ccrvg ccrvf fba xba)
    applyPolyShim ContraCCRVarianceType ccrvf ccrvg (MkPolyMapT (MkIsomorphism fab fba)) (MkCatDual (MkPolyMapT (MkIsomorphism xab xba))) =
        MkPolyMapT $ MkIsomorphism (applyContraPolyShim ccrvf ccrvg fab xab) (applyContraPolyShim ccrvg ccrvf fba xba)
    applyPolyShim RangeCCRVarianceType ccrvf ccrvg (MkPolyMapT (MkIsomorphism fab fba)) (MkCatRange (MkPolyMapT (MkIsomorphism xab1 xba1)) (MkPolyMapT (MkIsomorphism xab2 xba2))) =
        MkPolyMapT
            $ MkIsomorphism (applyRangePolyShim ccrvf ccrvg fab xab1 xab2) (applyRangePolyShim ccrvg ccrvf fba xba1 xba2)

instance
    forall (pshim :: PolyShimKind) m.
    (ApplyPolyShim pshim, Applicative m) =>
    ApplyPolyShim (PolyComposeShim m pshim)
    where
    applyPolyShim CoCCRVarianceType ccrvf ccrvg (MkPolyMapT (MkComposeShim mfab)) (MkPolyMapT (MkComposeShim mxab)) =
        MkPolyMapT $ MkComposeShim (liftA2 (applyCoPolyShim ccrvf ccrvg) mfab mxab)
    applyPolyShim ContraCCRVarianceType ccrvf ccrvg (MkPolyMapT (MkComposeShim mfab)) (MkCatDual (MkPolyMapT (MkComposeShim mxab))) =
        MkPolyMapT $ MkComposeShim ((\fab xab -> applyContraPolyShim ccrvf ccrvg fab xab) <$> mfab <*> mxab)
    applyPolyShim RangeCCRVarianceType ccrvf ccrvg (MkPolyMapT (MkComposeShim mfab)) (MkCatRange (MkPolyMapT (MkComposeShim mxab1)) (MkPolyMapT (MkComposeShim mxab2))) =
        MkPolyMapT
            $ MkComposeShim ((\fab xab1 xab2 -> applyRangePolyShim ccrvf ccrvg fab xab1 xab2) <$> mfab <*> mxab1 <*> mxab2)
