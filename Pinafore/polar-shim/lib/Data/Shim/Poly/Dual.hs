{-# OPTIONS -fno-warn-orphans #-}

module Data.Shim.Poly.Dual where

import Shapes

import Data.Shim.Mono
import Data.Shim.Poly.Map
import Data.Shim.Poly.Shim

type PolyDual :: PolyShimKind -> PolyShimKind
type PolyDual = PolyMapT CatDual

instance
    forall (pshim :: PolyShimKind) cat.
    CatFunctor (pshim Type) (pshim (Type -> Type)) cat =>
    CatFunctor (PolyDual pshim Type) (PolyDual pshim (Type -> Type)) cat
    where
    cfmap (MkPolyMapT (MkCatDual ba)) = MkPolyMapT $ MkCatDual $ cfmap ba

instance
    forall (pshim :: PolyShimKind) cat.
    CatFunctor (CatDual (pshim Type)) (pshim (Type -> Type)) cat =>
    CatFunctor (CatDual (PolyDual pshim Type)) (PolyDual pshim (Type -> Type)) cat
    where
    cfmap (MkCatDual (MkPolyMapT (MkCatDual ba))) = MkPolyMapT $ MkCatDual $ cfmap $ MkCatDual ba

instance
    forall (pshim :: PolyShimKind) k.
    (CoercibleKind k, IsoMapShim (pshim k), Category (pshim k)) =>
    IsoMapShim (PolyDual pshim k)
    where
    isoMapShim ::
        String ->
        (KindFunction pa pb -> KindFunction qa qb) ->
        (KindFunction pb pa -> KindFunction qb qa) ->
        PolyDual pshim k pa pb ->
        PolyDual pshim k qa qb
    isoMapShim t f1 f2 (MkPolyMapT (MkCatDual ba)) = MkPolyMapT $ MkCatDual $ isoMapShim t f2 f1 ba

instance
    forall (pshim :: PolyShimKind) k.
    (CoercibleKind k, CoerceShim (pshim k), Category (pshim k)) =>
    CoerceShim (PolyDual pshim k)
    where
    coercionToShim n c = MkPolyMapT $ MkCatDual $ coercionToShim n $ invert c
    shimToCoercion (MkPolyMapT (MkCatDual ba)) = fmap invert $ shimToCoercion ba

instance forall (pshim :: PolyShimKind). AllCategory pshim => ReduciblePolyShim (PolyDual pshim) where
    type ReducedPolyShim (PolyDual pshim) = PolyDual pshim
