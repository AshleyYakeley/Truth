{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -fno-warn-orphans #-}

module Data.Shim.PolarShimWit where

import Data.Shim.JoinMeet
import Data.Shim.PolarMap
import Data.Shim.Polarity
import Data.Shim.ShimWit
import Shapes

type PolarShimWit :: forall k. ShimKind k -> (k -> Type) -> Polarity -> k -> Type
type PolarShimWit shim wit polarity = ShimWit (PolarMap shim polarity) wit

mkPolarShimWit ::
       forall (k :: Type) (shim :: ShimKind k) wit polarity (t :: k).
       (InKind t, InCategory shim, Is PolarityType polarity)
    => wit t
    -> PolarShimWit shim wit polarity t
mkPolarShimWit =
    case polarityType @polarity of
        PositiveType -> mkShimWit
        NegativeType -> mkShimWit

mkPolarShimWitT ::
       forall (shim :: ShimKind Type) wit polarity (t :: Type). (InCategory shim, Is PolarityType polarity)
    => wit t
    -> PolarShimWit shim wit polarity t
mkPolarShimWitT = mkPolarShimWit

mkPosShimWit ::
       forall (k :: Type) (shim :: ShimKind k) wit (t :: k) (t' :: k). InKind t'
    => wit t'
    -> shim t t'
    -> PolarShimWit shim wit 'Positive t
mkPosShimWit t conv = MkShimWit t (MkPolarMap conv)

mkNegShimWit ::
       forall (k :: Type) (shim :: ShimKind k) wit (t :: k) (t' :: k). InKind t'
    => wit t'
    -> shim t' t
    -> PolarShimWit shim wit 'Negative t
mkNegShimWit t conv = MkShimWit t (MkPolarMap conv)

unPolarShimWit ::
       forall polarity k (shim :: ShimKind k) wit (t :: k) r.
       PolarShimWit shim wit polarity t
    -> (forall (t' :: k). wit t' -> PolarMap shim polarity t t' -> r)
    -> r
unPolarShimWit (MkShimWit t conv) cont = cont t conv

-- can't use these patterns in do-blocks (without MonadFail), due to
-- https://gitlab.haskell.org/ghc/ghc/issues/15681
pattern MkPosShimWit ::
        forall k (shim :: ShimKind k) (wit :: k -> Type) (t :: k) . () =>
        forall (t' :: k) . InKind t' =>
        wit t' -> shim t t' -> PolarShimWit shim wit 'Positive t

pattern MkPosShimWit wit conv = MkShimWit wit (MkPolarMap conv)

{-# COMPLETE MkPosShimWit #-}

pattern MkNegShimWit ::
        forall k (shim :: ShimKind k) (wit :: k -> Type) (t :: k) . () =>
        forall (t' :: k) . InKind t' =>
        wit t' -> shim t' t -> PolarShimWit shim wit 'Negative t

pattern MkNegShimWit wit conv = MkShimWit wit (MkPolarMap conv)

{-# COMPLETE MkNegShimWit #-}

unPosShimWit ::
       forall k (shim :: ShimKind k) wit (t :: k) r.
       PolarShimWit shim wit 'Positive t
    -> (forall (t' :: k). InKind t' => wit t' -> shim t t' -> r)
    -> r
unPosShimWit (MkShimWit t (MkPolarMap conv)) cont = cont t conv

unNegShimWit ::
       forall k (shim :: ShimKind k) wit (t :: k) r.
       PolarShimWit shim wit 'Negative t
    -> (forall (t' :: k). InKind t' => wit t' -> shim t' t -> r)
    -> r
unNegShimWit (MkShimWit t (MkPolarMap conv)) cont = cont t conv

mapPolarShimWit ::
       forall polarity (k :: Type) (shim :: ShimKind k) wit (a :: k) (b :: k).
       (InCategory shim, Is PolarityType polarity, InKind a, InKind b)
    => PolarMap shim polarity b a
    -> PolarShimWit shim wit polarity a
    -> PolarShimWit shim wit polarity b
mapPolarShimWit ab (MkShimWit t conv) = MkShimWit t $ conv <.> ab

mapPolarShimWitT ::
       forall polarity (shim :: ShimKind Type) wit (a :: Type) (b :: Type). (InCategory shim, Is PolarityType polarity)
    => PolarMap shim polarity b a
    -> PolarShimWit shim wit polarity a
    -> PolarShimWit shim wit polarity b
mapPolarShimWitT = mapPolarShimWit

mapPosShimWit ::
       forall (k :: Type) (shim :: ShimKind k) wit (a :: k) (b :: k). (InCategory shim, InKind a, InKind b)
    => shim b a
    -> PolarShimWit shim wit 'Positive a
    -> PolarShimWit shim wit 'Positive b
mapPosShimWit ab = mapPolarShimWit $ MkPolarMap ab

mapNegShimWit ::
       forall (k :: Type) (shim :: ShimKind k) wit (a :: k) (b :: k). (InCategory shim, InKind a, InKind b)
    => shim a b
    -> PolarShimWit shim wit 'Negative a
    -> PolarShimWit shim wit 'Negative b
mapNegShimWit ab = mapPolarShimWit $ MkPolarMap ab

instance forall (shim :: ShimKind Type) wit. InCategory shim =>
             CatFunctor (CatDual shim) (->) (PolarShimWit shim wit 'Positive) where
    cfmap (MkCatDual ab) = mapPosShimWit ab

instance forall (shim :: ShimKind Type) wit. InCategory shim => CatFunctor shim (->) (PolarShimWit shim wit 'Negative) where
    cfmap = mapNegShimWit

instance forall (shim :: ShimKind Type) wit polarity. (InCategory shim, Is PolarityType polarity) =>
             CatFunctor (CatDual (PolarMap shim polarity)) (->) (PolarShimWit shim wit polarity) where
    cfmap (MkCatDual (MkPolarMap ab)) =
        case polarityType @polarity of
            PositiveType -> cfmap $ MkCatDual ab
            NegativeType -> cfmap ab

chainPolarShimWitM ::
       forall m polarity (k :: Type) (shim :: ShimKind k) (wita :: k -> Type) (witb :: k -> Type) (t' :: k).
       (Functor m, InCategory shim, Is PolarityType polarity, InKind t')
    => (forall (t :: k). InKind t => wita t -> m (PolarShimWit shim witb polarity t))
    -> PolarShimWit shim wita polarity t'
    -> m (PolarShimWit shim witb polarity t')
chainPolarShimWitM f (MkShimWit t conv) = fmap (mapPolarShimWit conv) $ f t

chainPolarShimWit ::
       forall polarity (k :: Type) (shim :: ShimKind k) (wita :: k -> Type) (witb :: k -> Type) (t' :: k).
       (InCategory shim, Is PolarityType polarity, InKind t')
    => (forall (t :: k). InKind t => wita t -> PolarShimWit shim witb polarity t)
    -> PolarShimWit shim wita polarity t'
    -> PolarShimWit shim witb polarity t'
chainPolarShimWit f (MkShimWit t conv) = mapPolarShimWit conv $ f t

class ToPolarShimWit (shim :: ShimKind k) (wit :: k -> Type) (t :: k) where
    toPolarShimWit :: PolarShimWit shim wit 'Positive t

class FromPolarShimWit (shim :: ShimKind k) (wit :: k -> Type) (t :: k) where
    fromPolarShimWit :: PolarShimWit shim wit 'Negative t

type ToListShimWit (shim :: ShimKind k) wit = Is (ListType (Compose Dict (ToPolarShimWit shim wit)))

dictToShimWit ::
       forall k (shim :: ShimKind k) (wit :: k -> Type) (t :: k).
       Compose Dict (ToPolarShimWit shim wit) t
    -> PolarShimWit shim wit 'Positive t
dictToShimWit (Compose Dict) = toPolarShimWit

toListShimWit ::
       forall k (shim :: ShimKind k) (wit :: k -> Type) lt. ToListShimWit shim wit lt
    => ListType (PolarShimWit shim wit 'Positive) lt
toListShimWit = mapListType dictToShimWit representative

reshimWit ::
       forall polarity k (shim1 :: ShimKind k) (shim2 :: ShimKind k) (wit :: k -> Type) (t :: k).
       Is PolarityType polarity
    => (forall a' b'. shim1 a' b' -> shim2 a' b')
    -> PolarShimWit shim1 wit polarity t
    -> PolarShimWit shim2 wit polarity t
reshimWit f (MkShimWit wt conv) = MkShimWit wt $ reshimPolarMap f conv
