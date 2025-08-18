{-# OPTIONS -fno-warn-orphans #-}

module Data.Shim.Poly.Compose where

import Shapes

import Data.Shim.Mono
import Data.Shim.Polar
import Data.Shim.Poly.Map
import Data.Shim.Poly.Shim

type PolyComposeShim :: (Type -> Type) -> PolyShimKind -> PolyShimKind
type PolyComposeShim m = PolyMapT (ComposeShim m)

unPolyComposeShim ::
    forall (pshim :: PolyShimKind) (m :: Type -> Type) k (a :: k) (b :: k).
    PolyComposeShim m pshim k a b ->
    m (pshim k a b)
unPolyComposeShim tm = unComposeShim $ unPolyMapT tm

mkPolyComposeShim ::
    forall (pshim :: PolyShimKind) (m :: Type -> Type) k (a :: k) (b :: k).
    m (pshim k a b) ->
    PolyComposeShim m pshim k a b
mkPolyComposeShim ms = MkPolyMapT $ MkComposeShim ms

purePolyComposeShim ::
    forall (pshim :: PolyShimKind) (m :: Type -> Type) k (a :: k) (b :: k).
    Applicative m =>
    pshim k a b ->
    PolyComposeShim m pshim k a b
purePolyComposeShim ps = mkPolyComposeShim $ pure ps

purePolyComposeShimWit ::
    forall (pshim :: PolyShimKind) (m :: Type -> Type) polarity k (w :: k -> Type) (t :: k).
    (Applicative m, Is PolarityType polarity) =>
    PolarShimWit (pshim k) w polarity t ->
    PolarShimWit (PolyComposeShim m pshim k) w polarity t
purePolyComposeShimWit (MkShimWit wt (MkPolarShim conv)) =
    MkShimWit wt
        $ MkPolarShim
        $ case polarityType @polarity of
            PositiveType -> purePolyComposeShim conv
            NegativeType -> purePolyComposeShim conv

polarUnPolyComposeShim ::
    forall (pshim :: PolyShimKind) (m :: Type -> Type) k polarity (a :: k) (b :: k).
    (Is PolarityType polarity, Functor m) =>
    PolarShim (PolyComposeShim m pshim k) polarity a b ->
    m (PolarShim (pshim k) polarity a b)
polarUnPolyComposeShim (MkPolarShim mab) =
    case polarityType @polarity of
        PositiveType -> fmap MkPolarShim $ unPolyComposeShim mab
        NegativeType -> fmap MkPolarShim $ unPolyComposeShim mab

polarMkPolyComposeShim ::
    forall (pshim :: PolyShimKind) (m :: Type -> Type) k polarity (a :: k) (b :: k).
    (Is PolarityType polarity, Functor m) =>
    m (PolarShim (pshim k) polarity a b) ->
    PolarShim (PolyComposeShim m pshim k) polarity a b
polarMkPolyComposeShim mab =
    case polarityType @polarity of
        PositiveType -> MkPolarShim $ mkPolyComposeShim $ fmap unPolarShim mab
        NegativeType -> MkPolarShim $ mkPolyComposeShim $ fmap unPolarShim mab

type PolyFuncShim :: Type -> PolyShimKind -> PolyShimKind
type PolyFuncShim t = PolyComposeShim ((->) t)

applyPolarPolyFuncShim ::
    forall k (pshim :: PolyShimKind) (t :: Type) polarity (a :: k) (b :: k).
    Is PolarityType polarity =>
    PolarShim (PolyFuncShim t pshim k) polarity a b ->
    t ->
    PolarShim (pshim k) polarity a b
applyPolarPolyFuncShim (MkPolarShim tm) t =
    MkPolarShim
        $ case polarityType @polarity of
            PositiveType -> unPolyComposeShim tm t
            NegativeType -> unPolyComposeShim tm t

mkPolarPolyFuncShim ::
    forall k (pshim :: PolyShimKind) (t :: Type) polarity (a :: k) (b :: k).
    Is PolarityType polarity =>
    (t -> PolarShim (pshim k) polarity a b) ->
    PolarShim (PolyFuncShim t pshim k) polarity a b
mkPolarPolyFuncShim f =
    MkPolarShim
        $ case polarityType @polarity of
            PositiveType -> mkPolyComposeShim $ \t -> unPolarShim $ f t
            NegativeType -> mkPolyComposeShim $ \t -> unPolarShim $ f t

instance
    forall (pshim :: PolyShimKind) m.
    (ReduciblePolyShim pshim, Applicative m) =>
    ReduciblePolyShim (PolyComposeShim m pshim)
    where
    type ReducedPolyShim (PolyComposeShim m pshim) = ReducedPolyShim pshim
    reduceShim f (MkPolyMapT (MkComposeShim conv)) = MkPolyMapT $ MkComposeShim $ fmap (reduceShim f) conv
