{-# LANGUAGE BangPatterns #-}

module Data.Shim.ShimWit where

import Data.Shim.PolarMap
import Data.Shim.Polarity
import Data.Shim.PolyMap
import Shapes

type ShimWit :: forall k. MapKind k -> (k -> Type) -> Polarity -> k -> Type
data ShimWit shim wit polarity t where
    MkShimWit
        :: forall (polarity :: Polarity) (k :: Type) (shim :: MapKind k) (wit :: k -> Type) (t :: k) (t' :: k).
           InKind t'
        => wit t'
        -> PolarMap shim polarity t t'
        -> ShimWit shim wit polarity t

mkShimWit ::
       forall (k :: Type) (shim :: MapKind k) wit polarity (t :: k).
       (InKind t, InCategory shim, Is PolarityType polarity)
    => wit t
    -> ShimWit shim wit polarity t
mkShimWit t =
    case polarityType @polarity of
        PositiveType -> MkShimWit t cid
        NegativeType -> MkShimWit t cid

mkPosShimWit ::
       forall (k :: Type) (shim :: MapKind k) wit (t :: k) (t' :: k). InKind t'
    => wit t'
    -> shim t t'
    -> ShimWit shim wit 'Positive t
mkPosShimWit t conv = MkShimWit t (MkPolarMap conv)

mkNegShimWit ::
       forall (k :: Type) (shim :: MapKind k) wit (t :: k) (t' :: k). InKind t'
    => wit t'
    -> shim t' t
    -> ShimWit shim wit 'Negative t
mkNegShimWit t conv = MkShimWit t (MkPolarMap conv)

unShimWit ::
       forall polarity k (shim :: MapKind k) wit (t :: k) r.
       ShimWit shim wit polarity t
    -> (forall (t' :: k). wit t' -> PolarMap shim polarity t t' -> r)
    -> r
unShimWit (MkShimWit t conv) cont = cont t conv

-- can't use these patterns in do-blocks (without MonadFail), due to
-- https://gitlab.haskell.org/ghc/ghc/issues/15681
pattern MkPosShimWit ::
        forall k (shim :: MapKind k) (wit :: k -> Type) (t :: k) . () =>
        forall (t' :: k) . InKind t' =>
        wit t' -> shim t t' -> ShimWit shim wit 'Positive t

pattern MkPosShimWit wit conv = MkShimWit wit (MkPolarMap conv)

{-# COMPLETE MkPosShimWit #-}

pattern MkNegShimWit ::
        forall k (shim :: MapKind k) (wit :: k -> Type) (t :: k) . () =>
        forall (t' :: k) . InKind t' =>
        wit t' -> shim t' t -> ShimWit shim wit 'Negative t

pattern MkNegShimWit wit conv = MkShimWit wit (MkPolarMap conv)

{-# COMPLETE MkNegShimWit #-}

unPosShimWit ::
       forall k (shim :: MapKind k) wit (t :: k) r.
       ShimWit shim wit 'Positive t
    -> (forall (t' :: k). InKind t' => wit t' -> shim t t' -> r)
    -> r
unPosShimWit (MkShimWit t (MkPolarMap conv)) cont = cont t conv

unNegShimWit ::
       forall k (shim :: MapKind k) wit (t :: k) r.
       ShimWit shim wit 'Negative t
    -> (forall (t' :: k). InKind t' => wit t' -> shim t' t -> r)
    -> r
unNegShimWit (MkShimWit t (MkPolarMap conv)) cont = cont t conv

mapShimWit ::
       forall polarity (k :: Type) (shim :: MapKind k) wit (a :: k) (b :: k).
       (InCategory shim, Is PolarityType polarity, InKind a, InKind b)
    => PolarMap shim polarity b a
    -> ShimWit shim wit polarity a
    -> ShimWit shim wit polarity b
mapShimWit ab (MkShimWit t conv) = MkShimWit t $ conv <.> ab

mapPosShimWit ::
       forall (k :: Type) (shim :: MapKind k) wit (a :: k) (b :: k). (InCategory shim, InKind a, InKind b)
    => shim b a
    -> ShimWit shim wit 'Positive a
    -> ShimWit shim wit 'Positive b
mapPosShimWit ab = mapShimWit $ MkPolarMap ab

mapNegShimWit ::
       forall (k :: Type) (shim :: MapKind k) wit (a :: k) (b :: k). (InCategory shim, InKind a, InKind b)
    => shim a b
    -> ShimWit shim wit 'Negative a
    -> ShimWit shim wit 'Negative b
mapNegShimWit ab = mapShimWit $ MkPolarMap ab

shimWitToAnyW ::
       forall (k :: Type) (shim :: MapKind k) polarity (wit :: k -> Type) (a :: k).
       ShimWit shim wit polarity a
    -> AnyW wit
shimWitToAnyW (MkShimWit t _) = MkAnyW t

instance forall (shim :: MapKind Type) wit. InCategory shim =>
             CatFunctor (CatDual shim) (->) (ShimWit shim wit 'Positive) where
    cfmap (MkCatDual ab) = mapPosShimWit ab

instance forall (shim :: MapKind Type) wit. InCategory shim => CatFunctor shim (->) (ShimWit shim wit 'Negative) where
    cfmap = mapNegShimWit

instance forall (shim :: MapKind Type) wit polarity. (InCategory shim, Is PolarityType polarity) =>
             CatFunctor (CatDual (PolarMap shim polarity)) (->) (ShimWit shim wit polarity) where
    cfmap (MkCatDual (MkPolarMap ab)) =
        case polarityType @polarity of
            PositiveType -> cfmap $ MkCatDual ab
            NegativeType -> cfmap ab

chainShimWitM ::
       forall m polarity (k :: Type) (shim :: MapKind k) (wita :: k -> Type) (witb :: k -> Type) (t' :: k).
       (Monad m, InCategory shim, Is PolarityType polarity, InKind t')
    => (forall (t :: k). InKind t => wita t -> m (ShimWit shim witb polarity t))
    -> ShimWit shim wita polarity t'
    -> m (ShimWit shim witb polarity t')
chainShimWitM f (MkShimWit t conv) = do
    tf <- f t
    return $ mapShimWit conv tf

chainShimWit ::
       forall polarity (k :: Type) (shim :: MapKind k) (wita :: k -> Type) (witb :: k -> Type) (t' :: k).
       (InCategory shim, Is PolarityType polarity, InKind t')
    => (forall (t :: k). InKind t => wita t -> ShimWit shim witb polarity t)
    -> ShimWit shim wita polarity t'
    -> ShimWit shim witb polarity t'
chainShimWit f (MkShimWit t conv) = mapShimWit conv $ f t

class ToShimWit (shim :: MapKind k) (wit :: k -> Type) (t :: k) where
    toShimWit :: ShimWit shim wit 'Positive t

class FromShimWit (shim :: MapKind k) (wit :: k -> Type) (t :: k) where
    fromShimWit :: ShimWit shim wit 'Negative t

type ToListShimWit (shim :: MapKind k) wit = Is (ListType (Compose Dict (ToShimWit shim wit)))

dictToShimWit ::
       forall k (shim :: MapKind k) (wit :: k -> Type) (t :: k).
       Compose Dict (ToShimWit shim wit) t
    -> ShimWit shim wit 'Positive t
dictToShimWit (Compose Dict) = toShimWit

toListShimWit ::
       forall k (shim :: MapKind k) (wit :: k -> Type) lt. ToListShimWit shim wit lt
    => ListType (ShimWit shim wit 'Positive) lt
toListShimWit = mapListType dictToShimWit representative
