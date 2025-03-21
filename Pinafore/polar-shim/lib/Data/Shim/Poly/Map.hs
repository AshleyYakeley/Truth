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
    LazyCategory (f (pshim Type)) =>
    LazyCategory (PolyMapT f pshim Type)
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
