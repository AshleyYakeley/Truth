module Data.Shim.PolarJoinMeet where

import Data.Shim.JoinMeet
import Data.Shim.PolarMap
import Data.Shim.Polarity
import Shapes

type family LimitType polarity :: Type where
    LimitType 'Positive = BottomType
    LimitType 'Negative = TopType

type family JoinMeetType polarity :: Type -> Type -> Type where
    JoinMeetType 'Positive = JoinType
    JoinMeetType 'Negative = MeetType

iJoinMeetL1 ::
       forall pol shim t. (JoinMeetIsoCategory shim, Is PolarityType pol)
    => shim (JoinMeetType pol t (LimitType pol)) t
iJoinMeetL1 =
    case polarityType @pol of
        PositiveType -> iJoinL1
        NegativeType -> iMeetL1

iJoinMeetL2 ::
       forall pol shim t. (JoinMeetIsoCategory shim, Is PolarityType pol)
    => shim (JoinMeetType pol (LimitType pol) t) t
iJoinMeetL2 =
    case polarityType @pol of
        PositiveType -> iJoinL2
        NegativeType -> iMeetL2

iJoinMeetR1 ::
       forall pol shim t. (JoinMeetIsoCategory shim, Is PolarityType pol)
    => shim t (JoinMeetType pol t (LimitType pol))
iJoinMeetR1 =
    case polarityType @pol of
        PositiveType -> iJoinR1
        NegativeType -> iMeetR1

iJoinMeetR2 ::
       forall pol shim t. (JoinMeetIsoCategory shim, Is PolarityType pol)
    => shim t (JoinMeetType pol (LimitType pol) t)
iJoinMeetR2 =
    case polarityType @pol of
        PositiveType -> iJoinR2
        NegativeType -> iMeetR2

iPolarL1 ::
       forall (shim :: ShimKind Type) polarity a. (Is PolarityType polarity, JoinMeetIsoCategory shim)
    => PolarMap shim polarity (JoinMeetType polarity a (LimitType polarity)) a
iPolarL1 =
    case polarityType @polarity of
        PositiveType -> MkPolarMap iJoinL1
        NegativeType -> MkPolarMap iMeetR1

iPolarL2 ::
       forall (shim :: ShimKind Type) polarity a. (Is PolarityType polarity, JoinMeetIsoCategory shim)
    => PolarMap shim polarity (JoinMeetType polarity (LimitType polarity) a) a
iPolarL2 =
    case polarityType @polarity of
        PositiveType -> MkPolarMap iJoinL2
        NegativeType -> MkPolarMap iMeetR2

iPolarR1 ::
       forall (shim :: ShimKind Type) polarity a. (Is PolarityType polarity, JoinMeetIsoCategory shim)
    => PolarMap shim polarity a (JoinMeetType polarity a (LimitType polarity))
iPolarR1 =
    case polarityType @polarity of
        PositiveType -> MkPolarMap iJoinR1
        NegativeType -> MkPolarMap iMeetL1

iPolarPair ::
       forall (shim :: ShimKind Type) polarity a1 a2 b1 b2. (Is PolarityType polarity, JoinMeetIsoCategory shim)
    => PolarMap shim polarity a1 a2
    -> PolarMap shim polarity b1 b2
    -> PolarMap shim polarity (JoinMeetType polarity a1 b1) (JoinMeetType polarity a2 b2)
iPolarPair =
    case polarityType @polarity of
        PositiveType -> \(MkPolarMap ar) (MkPolarMap br) -> MkPolarMap $ iJoinPair ar br
        NegativeType -> \(MkPolarMap ar) (MkPolarMap br) -> MkPolarMap $ iMeetPair ar br

iPolarSwap ::
       forall (shim :: ShimKind Type) polarity a b. (Is PolarityType polarity, JoinMeetIsoCategory shim)
    => PolarMap shim polarity (JoinMeetType polarity a b) (JoinMeetType polarity b a)
iPolarSwap =
    case polarityType @polarity of
        PositiveType -> MkPolarMap iJoinSwap
        NegativeType -> MkPolarMap iMeetSwap

iPolarSwapL ::
       forall (shim :: ShimKind Type) polarity a b c. (Is PolarityType polarity, JoinMeetIsoCategory shim)
    => PolarMap shim polarity (JoinMeetType polarity (JoinMeetType polarity a b) c) (JoinMeetType polarity a (JoinMeetType polarity b c))
iPolarSwapL =
    case polarityType @polarity of
        PositiveType -> MkPolarMap iJoinSwapL
        NegativeType -> MkPolarMap iMeetSwapR

iPolarSwapR ::
       forall (shim :: ShimKind Type) polarity a b c. (Is PolarityType polarity, JoinMeetIsoCategory shim)
    => PolarMap shim polarity (JoinMeetType polarity a (JoinMeetType polarity b c)) (JoinMeetType polarity (JoinMeetType polarity a b) c)
iPolarSwapR =
    case polarityType @polarity of
        PositiveType -> MkPolarMap iJoinSwapR
        NegativeType -> MkPolarMap iMeetSwapL

polarLimit ::
       forall (shim :: ShimKind Type) polarity a. (Is PolarityType polarity, JoinMeetCategory shim)
    => PolarMap shim polarity (LimitType polarity) a
polarLimit =
    case polarityType @polarity of
        PositiveType -> MkPolarMap initf
        NegativeType -> MkPolarMap termf

polar1 ::
       forall (shim :: ShimKind Type) polarity a b. (Is PolarityType polarity, JoinMeetCategory shim)
    => PolarMap shim polarity a (JoinMeetType polarity a b)
polar1 =
    case polarityType @polarity of
        PositiveType -> MkPolarMap join1
        NegativeType -> MkPolarMap meet1

polar2 ::
       forall (shim :: ShimKind Type) polarity a b. (Is PolarityType polarity, JoinMeetCategory shim)
    => PolarMap shim polarity b (JoinMeetType polarity a b)
polar2 =
    case polarityType @polarity of
        PositiveType -> MkPolarMap join2
        NegativeType -> MkPolarMap meet2

polarF ::
       forall (shim :: ShimKind Type) polarity a b r. (Is PolarityType polarity, JoinMeetCategory shim)
    => PolarMap shim polarity a r
    -> PolarMap shim polarity b r
    -> PolarMap shim polarity (JoinMeetType polarity a b) r
polarF =
    case polarityType @polarity of
        PositiveType -> \(MkPolarMap ar) (MkPolarMap br) -> MkPolarMap $ joinf ar br
        NegativeType -> \(MkPolarMap ar) (MkPolarMap br) -> MkPolarMap $ meetf ar br
