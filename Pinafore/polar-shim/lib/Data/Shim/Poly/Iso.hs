{-# OPTIONS -fno-warn-orphans #-}

module Data.Shim.Poly.Iso where

import Shapes

import Data.Shim.Mono
import Data.Shim.Polar
import Data.Shim.Poly.Map
import Data.Shim.Poly.Shim

type PolyIso :: PolyShimKind -> PolyShimKind
type PolyIso = PolyMapT Isomorphism

polarPolyIso ::
    forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k).
    (Is PolarityType polarity, Category (pshim k)) =>
    PolarShim (PolyIso pshim k) polarity a b ->
    Isomorphism (pshim k) a b
polarPolyIso (MkPolarShim iab) =
    case polarityType @polarity of
        PositiveType -> unPolyMapT iab
        NegativeType -> invert $ unPolyMapT iab

isoPolarPoly ::
    forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k).
    (Is PolarityType polarity, Category (pshim k)) =>
    Isomorphism (pshim k) a b ->
    PolarShim (PolyIso pshim k) polarity a b
isoPolarPoly iab =
    MkPolarShim
        $ case polarityType @polarity of
            PositiveType -> MkPolyMapT iab
            NegativeType -> MkPolyMapT $ invert iab

polarPolyIsoForwards ::
    forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k).
    Is PolarityType polarity =>
    PolarShim (PolyIso pshim k) polarity a b ->
    PolarShim (pshim k) polarity a b
polarPolyIsoForwards (MkPolarShim iab) =
    MkPolarShim
        $ case polarityType @polarity of
            PositiveType -> isoForwards $ unPolyMapT iab
            NegativeType -> isoForwards $ unPolyMapT iab

polarPolyIsoBackwards ::
    forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k).
    Is PolarityType polarity =>
    PolarShim (PolyIso pshim k) polarity a b ->
    PolarShim (pshim k) polarity b a
polarPolyIsoBackwards (MkPolarShim iab) =
    MkPolarShim
        $ case polarityType @polarity of
            PositiveType -> isoBackwards $ unPolyMapT iab
            NegativeType -> isoBackwards $ unPolyMapT iab

polarPolyIsoPositive ::
    forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k).
    Is PolarityType polarity =>
    PolarShim (PolyIso pshim k) polarity a b ->
    pshim k a b
polarPolyIsoPositive (MkPolarShim iab) =
    case polarityType @polarity of
        PositiveType -> isoForwards $ unPolyMapT iab
        NegativeType -> isoBackwards $ unPolyMapT iab

polarPolyIsoNegative ::
    forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k).
    Is PolarityType polarity =>
    PolarShim (PolyIso pshim k) polarity a b ->
    pshim k b a
polarPolyIsoNegative (MkPolarShim iab) =
    case polarityType @polarity of
        PositiveType -> isoBackwards $ unPolyMapT iab
        NegativeType -> isoForwards $ unPolyMapT iab

mkPolyIsoShim ::
    forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k).
    Is PolarityType polarity =>
    pshim k a b ->
    pshim k b a ->
    PolarShim (PolyIso pshim k) polarity a b
mkPolyIsoShim pab pba =
    case polarityType @polarity of
        PositiveType -> MkPolarShim $ MkPolyMapT $ MkIsomorphism pab pba
        NegativeType -> MkPolarShim $ MkPolyMapT $ MkIsomorphism pba pab

polarPolyIsoPolar1 ::
    forall (pshim :: PolyShimKind) polarity (a :: Type).
    (JoinMeetIsoShim (pshim Type), Is PolarityType polarity) =>
    PolarShim (PolyIso pshim Type) polarity (JoinMeetType polarity a (LimitType polarity)) a
polarPolyIsoPolar1 =
    MkPolarShim
        $ case polarityType @polarity of
            PositiveType -> MkPolyMapT iJoinL1
            NegativeType -> MkPolyMapT iMeetR1

polyIsoForwards :: forall (pshim :: PolyShimKind) k (a :: k) (b :: k). PolyIso pshim k a b -> pshim k a b
polyIsoForwards iab = isoForwards $ unPolyMapT iab

polyIsoPolar ::
    forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k).
    Is PolarityType polarity =>
    PolyIso pshim k a b ->
    PolarShim (pshim k) polarity a b
polyIsoPolar iab =
    MkPolarShim
        $ case polarityType @polarity of
            PositiveType -> isoForwards $ unPolyMapT iab
            NegativeType -> isoBackwards $ unPolyMapT iab

instance
    forall (pshim :: PolyShimKind) cat.
    CatFunctor (pshim Type) (pshim (Type -> Type)) cat =>
    CatFunctor (PolyIso pshim Type) (PolyIso pshim (Type -> Type)) cat
    where
    cfmap (MkPolyMapT (MkIsomorphism ab ba)) = MkPolyMapT $ MkIsomorphism (cfmap ab) (cfmap ba)

instance
    forall (pshim :: PolyShimKind) cat.
    CatFunctor (CatDual (pshim Type)) (pshim (Type -> Type)) cat =>
    CatFunctor (CatDual (PolyIso pshim Type)) (PolyIso pshim (Type -> Type)) cat
    where
    cfmap (MkCatDual (MkPolyMapT (MkIsomorphism ab ba))) =
        MkPolyMapT $ MkIsomorphism (cfmap $ MkCatDual ab) (cfmap $ MkCatDual ba)

instance forall (pshim :: PolyShimKind). AllCategory pshim => ReduciblePolyShim (PolyIso pshim) where
    type ReducedPolyShim (PolyIso pshim) = PolyIso pshim
