{-# LANGUAGE BangPatterns #-}

{-# OPTIONS -fno-warn-orphans #-}

module Data.Shim.Mono.PolarShimWit where

import Shapes

import Data.Shim.Mono.General
import Data.Shim.Mono.Polar
import Data.Shim.Mono.ShimWit
import Data.Shim.Polar

type PolarShimWit :: forall k. ShimKind k -> (k -> Type) -> Polarity -> k -> Type
type PolarShimWit shim wit polarity = ShimWit (PolarShim shim polarity) wit

mkPolarShimWit ::
    forall (k :: Type) (shim :: ShimKind k) wit polarity (t :: k).
    (Category shim, Is PolarityType polarity) =>
    wit t ->
    PolarShimWit shim wit polarity t
mkPolarShimWit =
    case polarityType @polarity of
        PositiveType -> mkShimWit
        NegativeType -> mkShimWit

mkPolarShimWitT ::
    forall (shim :: ShimKind Type) wit polarity (t :: Type).
    (Category shim, Is PolarityType polarity) =>
    wit t ->
    PolarShimWit shim wit polarity t
mkPolarShimWitT = mkPolarShimWit

mkPosShimWit ::
    forall (k :: Type) (shim :: ShimKind k) wit (t :: k) (t' :: k).
    wit t' ->
    shim t t' ->
    PolarShimWit shim wit 'Positive t
mkPosShimWit t conv = MkShimWit t (MkPolarShim conv)

mkNegShimWit ::
    forall (k :: Type) (shim :: ShimKind k) wit (t :: k) (t' :: k).
    wit t' ->
    shim t' t ->
    PolarShimWit shim wit 'Negative t
mkNegShimWit t conv = MkShimWit t (MkPolarShim conv)

unPolarShimWit ::
    forall polarity k (shim :: ShimKind k) wit (t :: k) r.
    PolarShimWit shim wit polarity t ->
    (forall (t' :: k). wit t' -> PolarShim shim polarity t t' -> r) ->
    r
unPolarShimWit (MkShimWit t conv) cont = cont t conv

-- can't use these patterns in do-blocks (without MonadFail), due to
-- https://gitlab.haskell.org/ghc/ghc/issues/15681
pattern MkPosShimWit ::
    forall k (shim :: ShimKind k) (wit :: k -> Type) (t :: k).
    () =>
    ( forall (t' :: k).
      wit t' -> shim t t' -> PolarShimWit shim wit 'Positive t
    )
pattern MkPosShimWit wit conv = MkShimWit wit (MkPolarShim conv)

{-# COMPLETE MkPosShimWit #-}

pattern MkNegShimWit ::
    forall k (shim :: ShimKind k) (wit :: k -> Type) (t :: k).
    () =>
    ( forall (t' :: k).
      wit t' -> shim t' t -> PolarShimWit shim wit 'Negative t
    )
pattern MkNegShimWit wit conv = MkShimWit wit (MkPolarShim conv)

{-# COMPLETE MkNegShimWit #-}

unPosShimWit ::
    forall k (shim :: ShimKind k) wit (t :: k) r.
    PolarShimWit shim wit 'Positive t ->
    (forall (t' :: k). wit t' -> shim t t' -> r) ->
    r
unPosShimWit (MkShimWit t (MkPolarShim conv)) cont = cont t conv

unNegShimWit ::
    forall k (shim :: ShimKind k) wit (t :: k) r.
    PolarShimWit shim wit 'Negative t ->
    (forall (t' :: k). wit t' -> shim t' t -> r) ->
    r
unNegShimWit (MkShimWit t (MkPolarShim conv)) cont = cont t conv

mapPolarShimWit ::
    forall polarity (k :: Type) (shim :: ShimKind k) wit (a :: k) (b :: k).
    (Category shim, Is PolarityType polarity) =>
    PolarShim shim polarity b a ->
    PolarShimWit shim wit polarity a ->
    PolarShimWit shim wit polarity b
mapPolarShimWit = mapShimWit

mapPolarShimWitT ::
    forall polarity (shim :: ShimKind Type) wit (a :: Type) (b :: Type).
    (Category shim, Is PolarityType polarity) =>
    PolarShim shim polarity b a ->
    PolarShimWit shim wit polarity a ->
    PolarShimWit shim wit polarity b
mapPolarShimWitT = mapPolarShimWit

mapPosShimWit ::
    forall (k :: Type) (shim :: ShimKind k) wit (a :: k) (b :: k).
    Category shim =>
    shim b a ->
    PolarShimWit shim wit 'Positive a ->
    PolarShimWit shim wit 'Positive b
mapPosShimWit ab = mapPolarShimWit $ MkPolarShim ab

mapNegShimWit ::
    forall (k :: Type) (shim :: ShimKind k) wit (a :: k) (b :: k).
    Category shim =>
    shim a b ->
    PolarShimWit shim wit 'Negative a ->
    PolarShimWit shim wit 'Negative b
mapNegShimWit ab = mapPolarShimWit $ MkPolarShim ab

switchPolarShimWit ::
    forall polarity (k :: Type) (shim1 :: ShimKind k) (shim2 :: ShimKind k) wit (t :: k).
    Is PolarityType polarity =>
    (forall a b. shim1 a b -> shim2 a b) ->
    PolarShimWit shim1 wit polarity t ->
    PolarShimWit shim2 wit polarity t
switchPolarShimWit mf =
    switchShimWit
        $ case polarityType @polarity of
            PositiveType -> \(MkPolarShim conv) -> MkPolarShim $ mf conv
            NegativeType -> \(MkPolarShim conv) -> MkPolarShim $ mf conv

instance
    forall (shim :: ShimKind Type) wit.
    Category shim =>
    CatFunctor (CatDual shim) (->) (PolarShimWit shim wit 'Positive)
    where
    cfmap (MkCatDual ab) = mapPosShimWit ab

instance forall (shim :: ShimKind Type) wit. Category shim => CatFunctor shim (->) (PolarShimWit shim wit 'Negative) where
    cfmap = mapNegShimWit

instance
    forall (shim :: ShimKind Type) wit polarity.
    (Category shim, Is PolarityType polarity) =>
    CatFunctor (CatDual (PolarShim shim polarity)) (->) (PolarShimWit shim wit polarity)
    where
    cfmap (MkCatDual (MkPolarShim ab)) =
        case polarityType @polarity of
            PositiveType -> cfmap $ MkCatDual ab
            NegativeType -> cfmap ab

chainPolarShimWitM ::
    forall m polarity (k :: Type) (shim :: ShimKind k) (wita :: k -> Type) (witb :: k -> Type) (t' :: k).
    (Functor m, Category shim, Is PolarityType polarity) =>
    (forall (t :: k). wita t -> m (PolarShimWit shim witb polarity t)) ->
    PolarShimWit shim wita polarity t' ->
    m (PolarShimWit shim witb polarity t')
chainPolarShimWitM f (MkShimWit t conv) = fmap (mapPolarShimWit conv) $ f t

chainPolarShimWit ::
    forall polarity (k :: Type) (shim :: ShimKind k) (wita :: k -> Type) (witb :: k -> Type) (t' :: k).
    (Category shim, Is PolarityType polarity) =>
    (forall (t :: k). wita t -> PolarShimWit shim witb polarity t) ->
    PolarShimWit shim wita polarity t' ->
    PolarShimWit shim witb polarity t'
chainPolarShimWit f (MkShimWit t conv) = mapPolarShimWit conv $ f t

class ToPolarShimWit (shim :: ShimKind k) (wit :: k -> Type) (t :: k) where
    toPolarShimWit :: PolarShimWit shim wit 'Positive t

class FromPolarShimWit (shim :: ShimKind k) (wit :: k -> Type) (t :: k) where
    fromPolarShimWit :: PolarShimWit shim wit 'Negative t

type ToListShimWit (shim :: ShimKind k) wit = Is (ListType (Compose Dict (ToPolarShimWit shim wit)))

dictToShimWit ::
    forall k (shim :: ShimKind k) (wit :: k -> Type) (t :: k).
    Compose Dict (ToPolarShimWit shim wit) t ->
    PolarShimWit shim wit 'Positive t
dictToShimWit (Compose Dict) = toPolarShimWit

toListShimWit ::
    forall k (shim :: ShimKind k) (wit :: k -> Type) lt.
    ToListShimWit shim wit lt =>
    ListType (PolarShimWit shim wit 'Positive) lt
toListShimWit = mapListType dictToShimWit representative

reshimWit ::
    forall polarity k (shim1 :: ShimKind k) (shim2 :: ShimKind k) (wit :: k -> Type) (t :: k).
    Is PolarityType polarity =>
    (forall a' b'. shim1 a' b' -> shim2 a' b') ->
    PolarShimWit shim1 wit polarity t ->
    PolarShimWit shim2 wit polarity t
reshimWit f (MkShimWit wt conv) = MkShimWit wt $ reshimPolarShim f conv
