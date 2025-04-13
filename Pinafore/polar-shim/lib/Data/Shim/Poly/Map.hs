module Data.Shim.Poly.Map where

import Shapes

import Data.Shim.Mono
import Data.Shim.Poly.Shim

type MapPolyT :: (forall k. ShimKind k -> ShimKind k) -> PolyShimKind -> PolyShimKind
newtype MapPolyT f pshim k a b = MkMapPolyT
    { unMapPolyT :: f (pshim k) a b
    }

instance
    forall (f :: forall k. ShimKind k -> ShimKind k) (pshim :: PolyShimKind) (k :: Type).
    Category (f (pshim k)) =>
    Category (MapPolyT f pshim k)
    where
    id = MkMapPolyT id
    MkMapPolyT p . MkMapPolyT q = MkMapPolyT $ p . q

instance
    forall (f :: forall k. ShimKind k -> ShimKind k) (pshim :: PolyShimKind) (k :: Type).
    Groupoid (f (pshim k)) =>
    Groupoid (MapPolyT f pshim k)
    where
    invert (MkMapPolyT p) = MkMapPolyT $ invert p

instance
    forall (f :: forall k. ShimKind k -> ShimKind k) (pshim :: PolyShimKind).
    JoinMeetIsoShim (f (pshim Type)) =>
    JoinMeetIsoShim (MapPolyT f pshim Type)
    where
    iJoinL1 = MkMapPolyT iJoinL1
    iJoinL2 = MkMapPolyT iJoinL2
    iJoinR1 = MkMapPolyT iJoinR1
    iJoinR2 = MkMapPolyT iJoinR2
    iJoinPair (MkMapPolyT aconv) (MkMapPolyT bconv) = MkMapPolyT $ iJoinPair aconv bconv
    iJoinSwap = MkMapPolyT iJoinSwap
    iJoinSwapL = MkMapPolyT iJoinSwapL
    iJoinSwapR = MkMapPolyT iJoinSwapR
    iJoinSwap4 = MkMapPolyT iJoinSwap4
    iMeetL1 = MkMapPolyT iMeetL1
    iMeetL2 = MkMapPolyT iMeetL2
    iMeetR1 = MkMapPolyT iMeetR1
    iMeetR2 = MkMapPolyT iMeetR2
    iMeetPair (MkMapPolyT aconv) (MkMapPolyT bconv) = MkMapPolyT $ iMeetPair aconv bconv
    iMeetSwap = MkMapPolyT iMeetSwap
    iMeetSwapL = MkMapPolyT iMeetSwapL
    iMeetSwapR = MkMapPolyT iMeetSwapR
    iMeetSwap4 = MkMapPolyT iMeetSwap4

instance
    forall (f :: forall k. ShimKind k -> ShimKind k) (pshim :: PolyShimKind).
    JoinMeetShim (f (pshim Type)) =>
    JoinMeetShim (MapPolyT f pshim Type)
    where
    initf = MkMapPolyT initf
    termf = MkMapPolyT termf
    join1 = MkMapPolyT join1
    join2 = MkMapPolyT join2
    joinf ar br = MkMapPolyT $ joinf (unMapPolyT ar) (unMapPolyT br)
    meet1 = MkMapPolyT meet1
    meet2 = MkMapPolyT meet2
    meetf ra rb = MkMapPolyT $ meetf (unMapPolyT ra) (unMapPolyT rb)

instance
    forall (f :: forall k. ShimKind k -> ShimKind k) (pshim :: PolyShimKind).
    LazyCategory (f (pshim Type)) =>
    LazyCategory (MapPolyT f pshim Type)
    where
    iLazy (MkMapPolyT ab) = MkMapPolyT $ iLazy ab

instance
    forall (f :: forall k. ShimKind k -> ShimKind k) (pshim :: PolyShimKind).
    CartesianShim (f (pshim Type)) =>
    CartesianShim (MapPolyT f pshim Type)
    where
    funcShim (MkMapPolyT ab) (MkMapPolyT pq) = MkMapPolyT $ funcShim ab pq
    pairShim (MkMapPolyT ab) (MkMapPolyT pq) = MkMapPolyT $ pairShim ab pq
    eitherShim (MkMapPolyT ab) (MkMapPolyT pq) = MkMapPolyT $ eitherShim ab pq
