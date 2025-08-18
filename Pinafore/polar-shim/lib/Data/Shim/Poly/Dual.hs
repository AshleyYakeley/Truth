{-# OPTIONS -fno-warn-orphans #-}

module Data.Shim.Poly.Dual where

import Shapes

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

instance forall (pshim :: PolyShimKind). AllCategory pshim => ReduciblePolyShim (PolyDual pshim) where
    type ReducedPolyShim (PolyDual pshim) = PolyDual pshim
