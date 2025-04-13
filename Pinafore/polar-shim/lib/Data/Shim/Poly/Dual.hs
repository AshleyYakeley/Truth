{-# OPTIONS -fno-warn-orphans #-}

module Data.Shim.Poly.Dual where

import Shapes

import Data.Shim.Mono
import Data.Shim.Poly.Map
import Data.Shim.Poly.Shim

type DualPolyT :: PolyShimKind -> PolyShimKind
type DualPolyT = MapPolyT CatDual

instance
    forall (pshim :: PolyShimKind) cat.
    CatFunctor (pshim Type) (pshim (Type -> Type)) cat =>
    CatFunctor (DualPolyT pshim Type) (DualPolyT pshim (Type -> Type)) cat
    where
    cfmap (MkMapPolyT (MkCatDual ba)) = MkMapPolyT $ MkCatDual $ cfmap ba

instance
    forall (pshim :: PolyShimKind) cat.
    CatFunctor (CatDual (pshim Type)) (pshim (Type -> Type)) cat =>
    CatFunctor (CatDual (DualPolyT pshim Type)) (DualPolyT pshim (Type -> Type)) cat
    where
    cfmap (MkCatDual (MkMapPolyT (MkCatDual ba))) = MkMapPolyT $ MkCatDual $ cfmap $ MkCatDual ba

instance
    forall (pshim :: PolyShimKind) k.
    (CoercibleKind k, IsoMapShim (pshim k), Category (pshim k)) =>
    IsoMapShim (DualPolyT pshim k)
    where
    isoMapShim ::
        String ->
        (KindFunction pa pb -> KindFunction qa qb) ->
        (KindFunction pb pa -> KindFunction qb qa) ->
        DualPolyT pshim k pa pb ->
        DualPolyT pshim k qa qb
    isoMapShim t f1 f2 (MkMapPolyT (MkCatDual ba)) = MkMapPolyT $ MkCatDual $ isoMapShim t f2 f1 ba

instance
    forall (pshim :: PolyShimKind) k.
    (CoercibleKind k, CoerceShim (pshim k), Category (pshim k)) =>
    CoerceShim (DualPolyT pshim k)
    where
    coercionToShim c = MkMapPolyT $ MkCatDual $ coercionToShim $ invert c
    shimToCoercion (MkMapPolyT (MkCatDual ba)) = fmap invert $ shimToCoercion ba

instance forall (pshim :: PolyShimKind). AllCategory pshim => ReduciblePolyShim (DualPolyT pshim) where
    type ReducedPolyShim (DualPolyT pshim) = DualPolyT pshim
