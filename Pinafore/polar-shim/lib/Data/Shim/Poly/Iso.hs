{-# OPTIONS -fno-warn-orphans #-}

module Data.Shim.Poly.Iso where

import Shapes

import Data.Shim.Mono
import Data.Shim.Polar
import Data.Shim.Poly.Map
import Data.Shim.Poly.Shim

type IsoPolyT :: PolyShimKind -> PolyShimKind
type IsoPolyT = MapPolyT Isomorphism

polarIsoPolyT ::
    forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k).
    (Is PolarityType polarity, Category (pshim k)) =>
    PolarShim (IsoPolyT pshim k) polarity a b ->
    Isomorphism (pshim k) a b
polarIsoPolyT (MkPolarShim iab) =
    case polarityType @polarity of
        PositiveType -> unMapPolyT iab
        NegativeType -> invert $ unMapPolyT iab

isoPolarPoly ::
    forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k).
    (Is PolarityType polarity, Category (pshim k)) =>
    Isomorphism (pshim k) a b ->
    PolarShim (IsoPolyT pshim k) polarity a b
isoPolarPoly iab =
    MkPolarShim
        $ case polarityType @polarity of
            PositiveType -> MkMapPolyT iab
            NegativeType -> MkMapPolyT $ invert iab

polarIsoPolyTForwards ::
    forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k).
    Is PolarityType polarity =>
    PolarShim (IsoPolyT pshim k) polarity a b ->
    PolarShim (pshim k) polarity a b
polarIsoPolyTForwards (MkPolarShim iab) =
    MkPolarShim
        $ case polarityType @polarity of
            PositiveType -> isoForwards $ unMapPolyT iab
            NegativeType -> isoForwards $ unMapPolyT iab

polarIsoPolyTBackwards ::
    forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k).
    Is PolarityType polarity =>
    PolarShim (IsoPolyT pshim k) polarity a b ->
    PolarShim (pshim k) polarity b a
polarIsoPolyTBackwards (MkPolarShim iab) =
    MkPolarShim
        $ case polarityType @polarity of
            PositiveType -> isoBackwards $ unMapPolyT iab
            NegativeType -> isoBackwards $ unMapPolyT iab

polarIsoPolyTPositive ::
    forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k).
    Is PolarityType polarity =>
    PolarShim (IsoPolyT pshim k) polarity a b ->
    pshim k a b
polarIsoPolyTPositive (MkPolarShim iab) =
    case polarityType @polarity of
        PositiveType -> isoForwards $ unMapPolyT iab
        NegativeType -> isoBackwards $ unMapPolyT iab

polarIsoPolyTNegative ::
    forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k).
    Is PolarityType polarity =>
    PolarShim (IsoPolyT pshim k) polarity a b ->
    pshim k b a
polarIsoPolyTNegative (MkPolarShim iab) =
    case polarityType @polarity of
        PositiveType -> isoBackwards $ unMapPolyT iab
        NegativeType -> isoForwards $ unMapPolyT iab

mkIsoPolyShim ::
    forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k).
    Is PolarityType polarity =>
    pshim k a b ->
    pshim k b a ->
    PolarShim (IsoPolyT pshim k) polarity a b
mkIsoPolyShim pab pba =
    case polarityType @polarity of
        PositiveType -> MkPolarShim $ MkMapPolyT $ MkIsomorphism pab pba
        NegativeType -> MkPolarShim $ MkMapPolyT $ MkIsomorphism pba pab

polarIsoPolyTPolar1 ::
    forall (pshim :: PolyShimKind) polarity (a :: Type).
    (JoinMeetIsoShim (pshim Type), Is PolarityType polarity) =>
    PolarShim (IsoPolyT pshim Type) polarity (JoinMeetType polarity a (LimitType polarity)) a
polarIsoPolyTPolar1 =
    MkPolarShim
        $ case polarityType @polarity of
            PositiveType -> MkMapPolyT iJoinL1
            NegativeType -> MkMapPolyT iMeetR1

polyIsoForwards :: forall (pshim :: PolyShimKind) k (a :: k) (b :: k). IsoPolyT pshim k a b -> pshim k a b
polyIsoForwards iab = isoForwards $ unMapPolyT iab

polyIsoPolar ::
    forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k).
    Is PolarityType polarity =>
    IsoPolyT pshim k a b ->
    PolarShim (pshim k) polarity a b
polyIsoPolar iab =
    MkPolarShim
        $ case polarityType @polarity of
            PositiveType -> isoForwards $ unMapPolyT iab
            NegativeType -> isoBackwards $ unMapPolyT iab

instance
    forall (pshim :: PolyShimKind) cat.
    CatFunctor (pshim Type) (pshim (Type -> Type)) cat =>
    CatFunctor (IsoPolyT pshim Type) (IsoPolyT pshim (Type -> Type)) cat
    where
    cfmap (MkMapPolyT (MkIsomorphism ab ba)) = MkMapPolyT $ MkIsomorphism (cfmap ab) (cfmap ba)

instance
    forall (pshim :: PolyShimKind) cat.
    CatFunctor (CatDual (pshim Type)) (pshim (Type -> Type)) cat =>
    CatFunctor (CatDual (IsoPolyT pshim Type)) (IsoPolyT pshim (Type -> Type)) cat
    where
    cfmap (MkCatDual (MkMapPolyT (MkIsomorphism ab ba))) =
        MkMapPolyT $ MkIsomorphism (cfmap $ MkCatDual ab) (cfmap $ MkCatDual ba)

instance
    forall (pshim :: PolyShimKind) k.
    (CoercibleKind k, IsoMapShim (pshim k), Category (pshim k)) =>
    IsoMapShim (IsoPolyT pshim k)
    where
    isoMapShim ::
        String ->
        (KindFunction pa pb -> KindFunction qa qb) ->
        (KindFunction pb pa -> KindFunction qb qa) ->
        IsoPolyT pshim k pa pb ->
        IsoPolyT pshim k qa qb
    isoMapShim t f1 f2 (MkMapPolyT (MkIsomorphism ab ba)) =
        MkMapPolyT $ MkIsomorphism (isoMapShim t f1 f2 ab) (isoMapShim t f2 f1 ba)

instance
    forall (pshim :: PolyShimKind) k.
    (CoercibleKind k, CoerceShim (pshim k), Category (pshim k)) =>
    CoerceShim (IsoPolyT pshim k)
    where
    coercionToShim c = MkMapPolyT $ MkIsomorphism (coercionToShim c) (coercionToShim $ invert c)
    shimToCoercion (MkMapPolyT (MkIsomorphism ab ba)) = shimToCoercion ab <|> fmap invert (shimToCoercion ba)

instance forall (pshim :: PolyShimKind). AllCategory pshim => ReduciblePolyShim (IsoPolyT pshim) where
    type ReducedPolyShim (IsoPolyT pshim) = IsoPolyT pshim
