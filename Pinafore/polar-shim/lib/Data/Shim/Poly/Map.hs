module Data.Shim.Poly.Map where

import Shapes

import Data.Shim.Mono
import Data.Shim.Poly.Shim

type PolyMapT :: (forall k. ShimKind k -> ShimKind k) -> PolyShimKind -> PolyShimKind
newtype PolyMapT f pshim k a b = MkPolyMapT
    { unPolyMapT :: f (pshim k) a b
    }

instance
    forall (f :: forall k. ShimKind k -> ShimKind k) (pshim :: PolyShimKind) (k :: Type).
    Category (f (pshim k)) =>
    Category (PolyMapT f pshim k)
    where
    id = MkPolyMapT id
    MkPolyMapT p . MkPolyMapT q = MkPolyMapT $ p . q

instance
    forall (f :: forall k. ShimKind k -> ShimKind k) (pshim :: PolyShimKind) (k :: Type).
    Groupoid (f (pshim k)) =>
    Groupoid (PolyMapT f pshim k)
    where
    invert (MkPolyMapT p) = MkPolyMapT $ invert p

instance
    forall (f :: forall k. ShimKind k -> ShimKind k) (pshim :: PolyShimKind).
    JoinMeetIsoShim (f (pshim Type)) =>
    JoinMeetIsoShim (PolyMapT f pshim Type)
    where
    iJoinL1 = MkPolyMapT iJoinL1
    iJoinL2 = MkPolyMapT iJoinL2
    iJoinR1 = MkPolyMapT iJoinR1
    iJoinR2 = MkPolyMapT iJoinR2
    iJoinPair (MkPolyMapT aconv) (MkPolyMapT bconv) = MkPolyMapT $ iJoinPair aconv bconv
    iJoinSwap = MkPolyMapT iJoinSwap
    iJoinSwapL = MkPolyMapT iJoinSwapL
    iJoinSwapR = MkPolyMapT iJoinSwapR
    iJoinSwap4 = MkPolyMapT iJoinSwap4
    iMeetL1 = MkPolyMapT iMeetL1
    iMeetL2 = MkPolyMapT iMeetL2
    iMeetR1 = MkPolyMapT iMeetR1
    iMeetR2 = MkPolyMapT iMeetR2
    iMeetPair (MkPolyMapT aconv) (MkPolyMapT bconv) = MkPolyMapT $ iMeetPair aconv bconv
    iMeetSwap = MkPolyMapT iMeetSwap
    iMeetSwapL = MkPolyMapT iMeetSwapL
    iMeetSwapR = MkPolyMapT iMeetSwapR
    iMeetSwap4 = MkPolyMapT iMeetSwap4

instance
    forall (f :: forall k. ShimKind k -> ShimKind k) (pshim :: PolyShimKind).
    JoinMeetShim (f (pshim Type)) =>
    JoinMeetShim (PolyMapT f pshim Type)
    where
    initf = MkPolyMapT initf
    termf = MkPolyMapT termf
    join1 = MkPolyMapT join1
    join2 = MkPolyMapT join2
    joinf ar br = MkPolyMapT $ joinf (unPolyMapT ar) (unPolyMapT br)
    meet1 = MkPolyMapT meet1
    meet2 = MkPolyMapT meet2
    meetf ra rb = MkPolyMapT $ meetf (unPolyMapT ra) (unPolyMapT rb)

instance
    forall (f :: forall k. ShimKind k -> ShimKind k) (pshim :: PolyShimKind).
    LazyShim (f (pshim Type)) =>
    LazyShim (PolyMapT f pshim Type)
    where
    iLazy (MkPolyMapT ab) = MkPolyMapT $ iLazy ab

instance
    forall (f :: forall k. ShimKind k -> ShimKind k) (pshim :: PolyShimKind).
    CartesianShim (f (pshim Type)) =>
    CartesianShim (PolyMapT f pshim Type)
    where
    funcShim (MkPolyMapT ab) (MkPolyMapT pq) = MkPolyMapT $ funcShim ab pq
    pairShim (MkPolyMapT ab) (MkPolyMapT pq) = MkPolyMapT $ pairShim ab pq
    eitherShim (MkPolyMapT ab) (MkPolyMapT pq) = MkPolyMapT $ eitherShim ab pq

instance
    forall (f :: forall k. ShimKind k -> ShimKind k) (pshim :: PolyShimKind) k.
    (CoercibleKind k, IsoMapShim (f (pshim k))) => IsoMapShim (PolyMapT f pshim k)
    where
    isoMapShim n ab ba (MkPolyMapT fconv) = MkPolyMapT $ isoMapShim n ab ba fconv

instance
    forall (f :: forall k. ShimKind k -> ShimKind k) (pshim :: PolyShimKind) k.
    (CoercibleKind k, CoerceShim (f (pshim k))) =>
    CoerceShim (PolyMapT f pshim k)
    where
    coercionToShim n c = MkPolyMapT $ coercionToShim n c

instance
    forall (f :: forall k. ShimKind k -> ShimKind k) (pshim :: PolyShimKind) k.
    (CoercibleKind k, ToCoerceShim (f (pshim k))) =>
    ToCoerceShim (PolyMapT f pshim k)
    where
    shimToCoercion (MkPolyMapT fconv) = shimToCoercion fconv

instance
    forall (f :: forall k. ShimKind k -> ShimKind k) (pshim :: PolyShimKind) k.
    (CoercibleKind k, FunctionShim (f (pshim k))) =>
    FunctionShim (PolyMapT f pshim k)
    where
    functionToShim n f = MkPolyMapT $ functionToShim n f

instance
    forall (f :: forall k. ShimKind k -> ShimKind k) (pshim :: PolyShimKind) k.
    (CoercibleKind k, ToFunctionShim (f (pshim k))) =>
    ToFunctionShim (PolyMapT f pshim k)
    where
    shimToFunction (MkPolyMapT fconv) = shimToFunction fconv
