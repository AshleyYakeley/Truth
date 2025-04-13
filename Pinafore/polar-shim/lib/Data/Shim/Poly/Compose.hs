{-# OPTIONS -fno-warn-orphans #-}

module Data.Shim.Poly.Compose where

import Shapes

import Data.Shim.Mono
import Data.Shim.Polar
import Data.Shim.Poly.Map
import Data.Shim.Poly.Shim

type ComposePolyT :: (Type -> Type) -> PolyShimKind -> PolyShimKind
type ComposePolyT m = MapPolyT (ComposeShim m)

unComposePolyT ::
    forall (pshim :: PolyShimKind) (m :: Type -> Type) k (a :: k) (b :: k).
    ComposePolyT m pshim k a b ->
    m (pshim k a b)
unComposePolyT tm = unComposeShim $ unMapPolyT tm

mkComposePolyT ::
    forall (pshim :: PolyShimKind) (m :: Type -> Type) k (a :: k) (b :: k).
    m (pshim k a b) ->
    ComposePolyT m pshim k a b
mkComposePolyT ms = MkMapPolyT $ MkComposeShim ms

pureComposePolyT ::
    forall (pshim :: PolyShimKind) (m :: Type -> Type) k (a :: k) (b :: k).
    Applicative m =>
    pshim k a b ->
    ComposePolyT m pshim k a b
pureComposePolyT ps = mkComposePolyT $ pure ps

pureComposePolyTWit ::
    forall (pshim :: PolyShimKind) (m :: Type -> Type) polarity k (w :: k -> Type) (t :: k).
    (Applicative m, Is PolarityType polarity) =>
    PolarShimWit (pshim k) w polarity t ->
    PolarShimWit (ComposePolyT m pshim k) w polarity t
pureComposePolyTWit (MkShimWit wt (MkPolarShim conv)) =
    MkShimWit wt
        $ MkPolarShim
        $ case polarityType @polarity of
            PositiveType -> pureComposePolyT conv
            NegativeType -> pureComposePolyT conv

polarUnComposePolyT ::
    forall (pshim :: PolyShimKind) (m :: Type -> Type) k polarity (a :: k) (b :: k).
    (Is PolarityType polarity, Functor m) =>
    PolarShim (ComposePolyT m pshim k) polarity a b ->
    m (PolarShim (pshim k) polarity a b)
polarUnComposePolyT (MkPolarShim mab) =
    case polarityType @polarity of
        PositiveType -> fmap MkPolarShim $ unComposePolyT mab
        NegativeType -> fmap MkPolarShim $ unComposePolyT mab

polarMkComposePolyT ::
    forall (pshim :: PolyShimKind) (m :: Type -> Type) k polarity (a :: k) (b :: k).
    (Is PolarityType polarity, Functor m) =>
    m (PolarShim (pshim k) polarity a b) ->
    PolarShim (ComposePolyT m pshim k) polarity a b
polarMkComposePolyT mab =
    case polarityType @polarity of
        PositiveType -> MkPolarShim $ mkComposePolyT $ fmap unPolarShim mab
        NegativeType -> MkPolarShim $ mkComposePolyT $ fmap unPolarShim mab

type PolyFuncShim :: Type -> PolyShimKind -> PolyShimKind
type PolyFuncShim t = ComposePolyT ((->) t)

applyPolarPolyFuncShim ::
    forall k (pshim :: PolyShimKind) (t :: Type) polarity (a :: k) (b :: k).
    Is PolarityType polarity =>
    PolarShim (PolyFuncShim t pshim k) polarity a b ->
    t ->
    PolarShim (pshim k) polarity a b
applyPolarPolyFuncShim (MkPolarShim tm) t =
    MkPolarShim
        $ case polarityType @polarity of
            PositiveType -> unComposePolyT tm t
            NegativeType -> unComposePolyT tm t

mkPolarPolyFuncShim ::
    forall k (pshim :: PolyShimKind) (t :: Type) polarity (a :: k) (b :: k).
    Is PolarityType polarity =>
    (t -> PolarShim (pshim k) polarity a b) ->
    PolarShim (PolyFuncShim t pshim k) polarity a b
mkPolarPolyFuncShim f =
    MkPolarShim
        $ case polarityType @polarity of
            PositiveType -> mkComposePolyT $ \t -> unPolarShim $ f t
            NegativeType -> mkComposePolyT $ \t -> unPolarShim $ f t

instance
    forall (pshim :: PolyShimKind) m.
    (IsoMapShim (pshim Type), Applicative m) =>
    IsoMapShim (ComposePolyT m pshim Type)
    where
    isoMapShim ::
        String ->
        (KindFunction pa pb -> KindFunction qa qb) ->
        (KindFunction pb pa -> KindFunction qb qa) ->
        ComposePolyT m pshim Type pa pb ->
        ComposePolyT m pshim Type qa qb
    isoMapShim t f1 f2 (MkMapPolyT (MkComposeShim mab)) = MkMapPolyT $ MkComposeShim $ fmap (isoMapShim t f1 f2) mab

instance
    forall (pshim :: PolyShimKind) m.
    (ReduciblePolyShim pshim, Applicative m) =>
    ReduciblePolyShim (ComposePolyT m pshim)
    where
    type ReducedPolyShim (ComposePolyT m pshim) = ReducedPolyShim pshim
    reduceShim f (MkMapPolyT (MkComposeShim conv)) = MkMapPolyT $ MkComposeShim $ fmap (reduceShim f) conv
