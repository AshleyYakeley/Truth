{-# OPTIONS -fno-warn-orphans #-}

module Data.Shim.PolyComposeShim where

import Data.Shim.CatRange
import Data.Shim.ComposeShim
import Data.Shim.PolarMap
import Data.Shim.Polarity
import Data.Shim.PolyMap
import Data.Shim.PolyShim
import Data.Shim.ShimWit
import Data.Shim.Variance
import Shapes

type PolyComposeShim :: (Type -> Type) -> PolyShimKind -> PolyShimKind
type PolyComposeShim m = PolyMapT (ComposeShim m)

unPolyComposeShim ::
       forall (pshim :: PolyShimKind) (m :: Type -> Type) k (a :: k) (b :: k).
       PolyComposeShim m pshim k a b
    -> m (pshim k a b)
unPolyComposeShim tm = unComposeShim $ unPolyMapT tm

mkPolyComposeShim ::
       forall (pshim :: PolyShimKind) (m :: Type -> Type) k (a :: k) (b :: k).
       m (pshim k a b)
    -> PolyComposeShim m pshim k a b
mkPolyComposeShim ms = MkPolyMapT $ MkComposeShim ms

purePolyComposeShim ::
       forall (pshim :: PolyShimKind) (m :: Type -> Type) k (a :: k) (b :: k). Applicative m
    => pshim k a b
    -> PolyComposeShim m pshim k a b
purePolyComposeShim ps = mkPolyComposeShim $ pure ps

purePolyComposeShimWit ::
       forall (pshim :: PolyShimKind) (m :: Type -> Type) polarity k (w :: k -> Type) (t :: k).
       (Applicative m, Is PolarityType polarity)
    => ShimWit (pshim k) w polarity t
    -> ShimWit (PolyComposeShim m pshim k) w polarity t
purePolyComposeShimWit (MkShimWit wt (MkPolarMap conv)) =
    MkShimWit wt $
    MkPolarMap $
    case polarityType @polarity of
        PositiveType -> purePolyComposeShim conv
        NegativeType -> purePolyComposeShim conv

instance forall (pshim :: PolyShimKind) m. (ApplyPolyShim pshim, Applicative m) =>
             ApplyPolyShim (PolyComposeShim m pshim) where
    applyPolyShim CovarianceType (MkPolyMapT (MkComposeShim mfab)) (MkPolyMapT (MkComposeShim mxab)) =
        MkPolyMapT $ MkComposeShim (liftA2 (applyPolyShim CovarianceType) mfab mxab)
    applyPolyShim ContravarianceType (MkPolyMapT (MkComposeShim mfab)) (MkCatDual (MkPolyMapT (MkComposeShim mxab))) =
        MkPolyMapT $
        MkComposeShim ((\fab xab -> applyPolyShim ContravarianceType fab $ MkCatDual xab) <$> mfab <*> mxab)
    applyPolyShim RangevarianceType (MkPolyMapT (MkComposeShim mfab)) (MkCatRange (MkPolyMapT (MkComposeShim mxab1)) (MkPolyMapT (MkComposeShim mxab2))) =
        MkPolyMapT $
        MkComposeShim
            ((\fab xab1 xab2 -> applyPolyShim RangevarianceType fab (MkCatRange xab1 xab2)) <$> mfab <*> mxab1 <*> mxab2)

type PolyFuncShim :: Type -> PolyShimKind -> PolyShimKind
type PolyFuncShim t = PolyComposeShim ((->) t)

applyPolarPolyFuncShim ::
       forall k (pshim :: PolyShimKind) (t :: Type) polarity (a :: k) (b :: k). Is PolarityType polarity
    => PolarMap (PolyFuncShim t pshim k) polarity a b
    -> t
    -> PolarMap (pshim k) polarity a b
applyPolarPolyFuncShim (MkPolarMap tm) t =
    MkPolarMap $
    case polarityType @polarity of
        PositiveType -> unPolyComposeShim tm t
        NegativeType -> unPolyComposeShim tm t

mkPolarPolyFuncShim ::
       forall k (pshim :: PolyShimKind) (t :: Type) polarity (a :: k) (b :: k). Is PolarityType polarity
    => (t -> PolarMap (pshim k) polarity a b)
    -> PolarMap (PolyFuncShim t pshim k) polarity a b
mkPolarPolyFuncShim f =
    MkPolarMap $
    case polarityType @polarity of
        PositiveType -> mkPolyComposeShim $ \t -> unPolarMap $ f t
        NegativeType -> mkPolyComposeShim $ \t -> unPolarMap $ f t
