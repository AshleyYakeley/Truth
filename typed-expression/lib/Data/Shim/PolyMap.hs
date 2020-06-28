module Data.Shim.PolyMap where

import Data.Shim.JoinMeet
import Shapes

type PolyShimKind = forall k -> ShimKind k

type PolyMorphism :: ShimKind Type -> PolyShimKind
newtype PolyMorphism shim k a b =
    MkPolyMorphism (KindMorphism shim a b)

type PolyFunction :: PolyShimKind
type PolyFunction = PolyMorphism (->)

instance forall (shim :: ShimKind Type) k. InCategory (KindMorphism shim :: ShimKind k) =>
             InCategory (PolyMorphism shim k) where
    cid = MkPolyMorphism cid
    MkPolyMorphism p <.> MkPolyMorphism q = MkPolyMorphism $ p <.> q

instance forall (shim :: ShimKind Type) k. InGroupoid (KindMorphism shim :: ShimKind k) =>
             InGroupoid (PolyMorphism shim k) where
    cinvert (MkPolyMorphism p) = MkPolyMorphism $ cinvert p

instance InCategory (KindMorphism shim :: ShimKind Type) => Category (PolyMorphism shim Type) where
    id = cid
    (.) = (<.>)

instance InGroupoid (KindMorphism shim :: ShimKind Type) => Groupoid (PolyMorphism shim Type) where
    invert = cinvert

type PolyMapT :: (forall k. ShimKind k -> ShimKind k) -> PolyShimKind -> PolyShimKind
newtype PolyMapT f pmap k a b = MkPolyMapT
    { unPolyMapT :: f (pmap k) a b
    }

instance forall (f :: forall k. ShimKind k -> ShimKind k) (pshim :: PolyShimKind) (k :: Type). InCategory (f (pshim k)) =>
             InCategory (PolyMapT f pshim k) where
    cid = MkPolyMapT cid
    MkPolyMapT p <.> MkPolyMapT q = MkPolyMapT $ p <.> q

instance forall (f :: forall k. ShimKind k -> ShimKind k) (pshim :: PolyShimKind) (k :: Type). InGroupoid (f (pshim k)) =>
             InGroupoid (PolyMapT f pshim k) where
    cinvert (MkPolyMapT p) = MkPolyMapT $ cinvert p

instance forall (f :: forall k. ShimKind k -> ShimKind k) (pshim :: PolyShimKind) (k :: Type). Category (f (pshim k)) =>
             Category (PolyMapT f pshim k) where
    id = MkPolyMapT id
    MkPolyMapT p . MkPolyMapT q = MkPolyMapT $ p . q

instance forall (f :: forall k. ShimKind k -> ShimKind k) (pshim :: PolyShimKind) (k :: Type). Groupoid (f (pshim k)) =>
             Groupoid (PolyMapT f pshim k) where
    invert (MkPolyMapT p) = MkPolyMapT $ invert p

instance forall (f :: forall k. ShimKind k -> ShimKind k) (pshim :: PolyShimKind). JoinMeetIsoCategory (f (pshim Type)) =>
             JoinMeetIsoCategory (PolyMapT f pshim Type) where
    iJoinL1 = MkPolyMapT iJoinL1
    iJoinL2 = MkPolyMapT iJoinL2
    iJoinR1 = MkPolyMapT iJoinR1
    iJoinR2 = MkPolyMapT iJoinR2
    iJoinPair (MkPolyMapT aconv) (MkPolyMapT bconv) = MkPolyMapT $ iJoinPair aconv bconv
    iJoinSwap = MkPolyMapT iJoinSwap
    iJoinSwapL = MkPolyMapT iJoinSwapL
    iJoinSwapR = MkPolyMapT iJoinSwapR
    iMeetL1 = MkPolyMapT iMeetL1
    iMeetL2 = MkPolyMapT iMeetL2
    iMeetR1 = MkPolyMapT iMeetR1
    iMeetR2 = MkPolyMapT iMeetR2
    iMeetPair (MkPolyMapT aconv) (MkPolyMapT bconv) = MkPolyMapT $ iMeetPair aconv bconv
    iMeetSwap = MkPolyMapT iMeetSwap
    iMeetSwapL = MkPolyMapT iMeetSwapL
    iMeetSwapR = MkPolyMapT iMeetSwapR
