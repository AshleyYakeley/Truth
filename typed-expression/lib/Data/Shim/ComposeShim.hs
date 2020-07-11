module Data.Shim.ComposeShim where

import Data.Shim.JoinMeet
import Shapes

type ComposeShim :: (Type -> Type) -> forall k. ShimKind k -> ShimKind k
newtype ComposeShim m shim a b = MkComposeShim
    { unComposeShim :: m (shim a b)
    }

instance forall (m :: Type -> Type) k (shim :: ShimKind k). (Applicative m, Category shim) =>
             Category (ComposeShim m shim) where
    id = MkComposeShim $ pure id
    MkComposeShim mp . MkComposeShim mq = MkComposeShim $ liftA2 (.) mp mq

instance forall (m :: Type -> Type) k (shim :: ShimKind k). (Applicative m, InCategory shim) =>
             InCategory (ComposeShim m shim) where
    cid = MkComposeShim $ pure cid
    MkComposeShim mp <.> MkComposeShim mq = MkComposeShim $ liftA2 (<.>) mp mq

instance forall (m :: Type -> Type) (shim :: ShimKind Type). (Applicative m, JoinMeetIsoCategory shim) =>
             JoinMeetIsoCategory (ComposeShim m shim) where
    iJoinL1 = MkComposeShim $ pure iJoinL1
    iJoinL2 = MkComposeShim $ pure iJoinL2
    iJoinR1 = MkComposeShim $ pure iJoinR1
    iJoinR2 = MkComposeShim $ pure iJoinR2
    iJoinPair (MkComposeShim maconv) (MkComposeShim mbconv) = MkComposeShim $ liftA2 iJoinPair maconv mbconv
    iJoinSwap = MkComposeShim $ pure iJoinSwap
    iJoinSwapL = MkComposeShim $ pure iJoinSwapL
    iJoinSwapR = MkComposeShim $ pure iJoinSwapR
    iMeetL1 = MkComposeShim $ pure iMeetL1
    iMeetL2 = MkComposeShim $ pure iMeetL2
    iMeetR1 = MkComposeShim $ pure iMeetR1
    iMeetR2 = MkComposeShim $ pure iMeetR2
    iMeetPair (MkComposeShim maconv) (MkComposeShim mbconv) = MkComposeShim $ liftA2 iMeetPair maconv mbconv
    iMeetSwap = MkComposeShim $ pure iMeetSwap
    iMeetSwapL = MkComposeShim $ pure iMeetSwapL
    iMeetSwapR = MkComposeShim $ pure iMeetSwapR

instance forall (m :: Type -> Type) (shim :: ShimKind Type). (Applicative m, JoinMeetCategory shim) =>
             JoinMeetCategory (ComposeShim m shim) where
    initf = MkComposeShim $ pure initf
    termf = MkComposeShim $ pure termf
    join1 = MkComposeShim $ pure join1
    join2 = MkComposeShim $ pure join2
    joinf (MkComposeShim maconv) (MkComposeShim mbconv) = MkComposeShim $ liftA2 joinf maconv mbconv
    meet1 = MkComposeShim $ pure meet1
    meet2 = MkComposeShim $ pure meet2
    meetf (MkComposeShim maconv) (MkComposeShim mbconv) = MkComposeShim $ liftA2 meetf maconv mbconv
    applf (MkComposeShim maconv) (MkComposeShim mbconv) = MkComposeShim $ liftA2 applf maconv mbconv
