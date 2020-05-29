{-# LANGUAGE BangPatterns #-}

module Data.Shim.ShimWit where

import Data.Shim.PolarMap
import Data.Shim.Polarity
import Shapes

data ShimWit (shim :: k -> k -> Type) (wit :: k -> Type) (polarity :: Polarity) (t :: k) :: Type where
    MkShimWit
        :: forall (k :: Type) (shim :: k -> k -> Type) wit polarity (t :: k) (t' :: k). InKind t'
        => wit t'
        -> PolarMap shim polarity t t'
        -> ShimWit shim wit polarity t

mkShimWit ::
       forall (k :: Type) (shim :: k -> k -> Type) polarity wit (t :: k).
       (InKind t, InCategory shim, Is PolarityType polarity)
    => wit t
    -> ShimWit shim wit polarity t
mkShimWit t =
    case polarityType @polarity of
        PositiveType -> MkShimWit t cid
        NegativeType -> MkShimWit t cid

mkPosShimWit ::
       forall (k :: Type) (shim :: k -> k -> Type) wit (t :: k) (t' :: k). InKind t'
    => wit t'
    -> shim t t'
    -> ShimWit shim wit 'Positive t
mkPosShimWit t conv = MkShimWit t (MkPolarMap conv)

mkNegShimWit ::
       forall (k :: Type) (shim :: k -> k -> Type) wit (t :: k) (t' :: k). InKind t'
    => wit t'
    -> shim t' t
    -> ShimWit shim wit 'Negative t
mkNegShimWit t conv = MkShimWit t (MkPolarMap conv)

unShimWit :: ShimWit shim wit polarity t -> (forall t'. wit t' -> PolarMap shim polarity t t' -> r) -> r
unShimWit (MkShimWit t conv) cont = cont t conv

-- can't use these patterns in do-blocks (without MonadFail), due to
-- https://gitlab.haskell.org/ghc/ghc/issues/15681
pattern MkPosShimWit :: forall shim wit t . () => forall t' .
        InKind t' => wit t' -> shim t t' -> ShimWit shim wit 'Positive t

pattern MkPosShimWit wit conv = MkShimWit wit (MkPolarMap conv)

{-# COMPLETE MkPosShimWit #-}

pattern MkNegShimWit :: forall shim wit t . () => forall t' .
        InKind t' => wit t' -> shim t' t -> ShimWit shim wit 'Negative t

pattern MkNegShimWit wit conv = MkShimWit wit (MkPolarMap conv)

{-# COMPLETE MkNegShimWit #-}

unPosShimWit :: ShimWit shim wit 'Positive t -> (forall t'. InKind t' => wit t' -> shim t t' -> r) -> r
unPosShimWit (MkShimWit t (MkPolarMap conv)) cont = cont t conv

unNegShimWit :: ShimWit shim wit 'Negative t -> (forall t'. InKind t' => wit t' -> shim t' t -> r) -> r
unNegShimWit (MkShimWit t (MkPolarMap conv)) cont = cont t conv

mapShimWit ::
       forall (k :: Type) (shim :: k -> k -> Type) wit polarity (a :: k) (b :: k).
       (InCategory shim, Is PolarityType polarity, InKind a, InKind b)
    => PolarMap shim polarity b a
    -> ShimWit shim wit polarity a
    -> ShimWit shim wit polarity b
mapShimWit ab (MkShimWit t conv) = MkShimWit t $ conv <.> ab

mapPosShimWit ::
       forall (k :: Type) (shim :: k -> k -> Type) wit (a :: k) (b :: k). (InCategory shim, InKind a, InKind b)
    => shim b a
    -> ShimWit shim wit 'Positive a
    -> ShimWit shim wit 'Positive b
mapPosShimWit ab = mapShimWit $ MkPolarMap ab

mapNegShimWit ::
       forall (k :: Type) (shim :: k -> k -> Type) wit (a :: k) (b :: k). (InCategory shim, InKind a, InKind b)
    => shim a b
    -> ShimWit shim wit 'Negative a
    -> ShimWit shim wit 'Negative b
mapNegShimWit ab = mapShimWit $ MkPolarMap ab

instance Contravariant (ShimWit (->) wit 'Positive) where
    contramap = mapPosShimWit

instance InCategory shim => CatFunctor (CatDual shim) (->) (ShimWit shim wit 'Positive) where
    cfmap (MkCatDual ab) = mapPosShimWit ab

instance Functor (ShimWit (->) wit 'Negative) where
    fmap = mapNegShimWit

instance InCategory shim => CatFunctor shim (->) (ShimWit shim wit 'Negative) where
    cfmap = mapNegShimWit

instance (InCategory shim, Is PolarityType polarity) =>
             CatFunctor (CatDual (PolarMap shim polarity)) (->) (ShimWit shim wit polarity) where
    cfmap (MkCatDual (MkPolarMap ab)) =
        case polarityType @polarity of
            PositiveType -> cfmap $ MkCatDual ab
            NegativeType -> cfmap ab

chainShimWitM ::
       forall m (k :: Type) (shim :: k -> k -> Type) (wita :: k -> Type) (witb :: k -> Type) polarity (t' :: k).
       (Monad m, InCategory shim, Is PolarityType polarity, InKind t')
    => (forall (t :: k). InKind t => wita t -> m (ShimWit shim witb polarity t))
    -> ShimWit shim wita polarity t'
    -> m (ShimWit shim witb polarity t')
chainShimWitM f (MkShimWit t conv) = do
    tf <- f t
    return $ mapShimWit conv tf

chainShimWit ::
       forall (k :: Type) (shim :: k -> k -> Type) (wita :: k -> Type) (witb :: k -> Type) polarity (t' :: k).
       (InCategory shim, Is PolarityType polarity, InKind t')
    => (forall (t :: k). InKind t => wita t -> ShimWit shim witb polarity t)
    -> ShimWit shim wita polarity t'
    -> ShimWit shim witb polarity t'
chainShimWit f (MkShimWit t conv) = mapShimWit conv $ f t

class ToShimWit (shim :: k -> k -> Type) (wit :: k -> Type) (t :: k) where
    toShimWit :: ShimWit shim wit 'Positive t

class FromShimWit (shim :: k -> k -> Type) (wit :: k -> Type) (t :: k) where
    fromShimWit :: ShimWit shim wit 'Negative t

type ToListShimWit shim wit = Is (ListType (Compose Dict (ToShimWit shim wit)))

dictToShimWit :: Compose Dict (ToShimWit shim wit) t -> ShimWit shim wit 'Positive t
dictToShimWit (Compose Dict) = toShimWit

toListShimWit :: ToListShimWit shim wit lt => ListType (ShimWit shim wit 'Positive) lt
toListShimWit = mapListType dictToShimWit representative
