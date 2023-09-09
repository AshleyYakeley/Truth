{-# OPTIONS -fno-warn-orphans #-}

module Data.Shim.PolyDual where

import Data.Shim.CCRVariance
import Data.Shim.CatRange
import Data.Shim.JoinMeet
import Data.Shim.PolyMap
import Data.Shim.PolyShim
import Shapes

type PolyDual :: PolyShimKind -> PolyShimKind
type PolyDual = PolyMapT CatDual

instance forall (pshim :: PolyShimKind). ApplyPolyShim pshim => ApplyPolyShim (PolyDual pshim) where
    applyPolyShim CoCCRVarianceType ccrvf ccrvg (MkPolyMapT (MkCatDual fba)) (MkPolyMapT (MkCatDual xba)) =
        MkPolyMapT $ MkCatDual $ applyCoPolyShim ccrvg ccrvf fba xba
    applyPolyShim ContraCCRVarianceType ccrvf ccrvg (MkPolyMapT (MkCatDual fba)) (MkCatDual (MkPolyMapT (MkCatDual xba))) =
        MkPolyMapT $ MkCatDual $ applyContraPolyShim ccrvg ccrvf fba xba
    applyPolyShim RangeCCRVarianceType ccrvf ccrvg (MkPolyMapT (MkCatDual fba)) (MkCatRange (MkPolyMapT (MkCatDual xba1)) (MkPolyMapT (MkCatDual xba2))) =
        MkPolyMapT $ MkCatDual $ applyRangePolyShim ccrvg ccrvf fba xba1 xba2

instance forall (pshim :: PolyShimKind) cat. CatFunctor (pshim Type) (pshim (Type -> Type)) cat =>
             CatFunctor (PolyDual pshim Type) (PolyDual pshim (Type -> Type)) cat where
    cfmap (MkPolyMapT (MkCatDual ba)) = MkPolyMapT $ MkCatDual $ cfmap ba

instance forall (pshim :: PolyShimKind) cat. CatFunctor (CatDual (pshim Type)) (pshim (Type -> Type)) cat =>
             CatFunctor (CatDual (PolyDual pshim Type)) (PolyDual pshim (Type -> Type)) cat where
    cfmap (MkCatDual (MkPolyMapT (MkCatDual ba))) = MkPolyMapT $ MkCatDual $ cfmap $ MkCatDual ba

instance forall (pshim :: PolyShimKind) k. (CoercibleKind k, IsoMapShim (pshim k), Category (pshim k)) =>
             IsoMapShim (PolyDual pshim k) where
    isoMapShim ::
           String
        -> (KindFunction pa pb -> KindFunction qa qb)
        -> (KindFunction pb pa -> KindFunction qb qa)
        -> PolyDual pshim k pa pb
        -> PolyDual pshim k qa qb
    isoMapShim t f1 f2 (MkPolyMapT (MkCatDual ba)) = MkPolyMapT $ MkCatDual $ isoMapShim t f2 f1 ba

instance forall (pshim :: PolyShimKind) k. (CoercibleKind k, CoerceShim (pshim k), Category (pshim k)) =>
             CoerceShim (PolyDual pshim k) where
    coercionToShim n c = MkPolyMapT $ MkCatDual $ coercionToShim n $ invert c
    shimToCoercion (MkPolyMapT (MkCatDual ba)) = fmap invert $ shimToCoercion ba

instance forall (pshim :: PolyShimKind). AllCategory pshim => ReduciblePolyShim (PolyDual pshim) where
    type ReducedPolyShim (PolyDual pshim) = PolyDual pshim
