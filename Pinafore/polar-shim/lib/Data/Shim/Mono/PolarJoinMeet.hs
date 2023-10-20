module Data.Shim.Mono.PolarJoinMeet where

import Data.Shim.Mono.General
import Data.Shim.Mono.Polar
import Data.Shim.Polar
import Shapes

type family LimitType polarity :: Type where
    LimitType 'Positive = BottomType
    LimitType 'Negative = TopType

type family JoinMeetType polarity :: Type -> Type -> Type where
    JoinMeetType 'Positive = JoinType
    JoinMeetType 'Negative = MeetType

iJoinMeetL1 ::
       forall (shim :: ShimKind Type) polarity t. (JoinMeetIsoShim shim, Is PolarityType polarity)
    => shim (JoinMeetType polarity t (LimitType polarity)) t
iJoinMeetL1 =
    case polarityType @polarity of
        PositiveType -> iJoinL1
        NegativeType -> iMeetL1

iJoinMeetL2 ::
       forall (shim :: ShimKind Type) polarity t. (JoinMeetIsoShim shim, Is PolarityType polarity)
    => shim (JoinMeetType polarity (LimitType polarity) t) t
iJoinMeetL2 =
    case polarityType @polarity of
        PositiveType -> iJoinL2
        NegativeType -> iMeetL2

iJoinMeetR1 ::
       forall (shim :: ShimKind Type) polarity t. (JoinMeetIsoShim shim, Is PolarityType polarity)
    => shim t (JoinMeetType polarity t (LimitType polarity))
iJoinMeetR1 =
    case polarityType @polarity of
        PositiveType -> iJoinR1
        NegativeType -> iMeetR1

iJoinMeetR2 ::
       forall (shim :: ShimKind Type) polarity t. (JoinMeetIsoShim shim, Is PolarityType polarity)
    => shim t (JoinMeetType polarity (LimitType polarity) t)
iJoinMeetR2 =
    case polarityType @polarity of
        PositiveType -> iJoinR2
        NegativeType -> iMeetR2

iPolarL1 ::
       forall (shim :: ShimKind Type) polarity a. (JoinMeetIsoShim shim, Is PolarityType polarity)
    => PolarShim shim polarity (JoinMeetType polarity a (LimitType polarity)) a
iPolarL1 =
    case polarityType @polarity of
        PositiveType -> MkPolarShim iJoinL1
        NegativeType -> MkPolarShim iMeetR1

iPolarL2 ::
       forall (shim :: ShimKind Type) polarity a. (JoinMeetIsoShim shim, Is PolarityType polarity)
    => PolarShim shim polarity (JoinMeetType polarity (LimitType polarity) a) a
iPolarL2 =
    case polarityType @polarity of
        PositiveType -> MkPolarShim iJoinL2
        NegativeType -> MkPolarShim iMeetR2

iPolarR1 ::
       forall (shim :: ShimKind Type) polarity a. (JoinMeetIsoShim shim, Is PolarityType polarity)
    => PolarShim shim polarity a (JoinMeetType polarity a (LimitType polarity))
iPolarR1 =
    case polarityType @polarity of
        PositiveType -> MkPolarShim iJoinR1
        NegativeType -> MkPolarShim iMeetL1

iPolarR2 ::
       forall (shim :: ShimKind Type) polarity a. (JoinMeetIsoShim shim, Is PolarityType polarity)
    => PolarShim shim polarity a (JoinMeetType polarity (LimitType polarity) a)
iPolarR2 =
    case polarityType @polarity of
        PositiveType -> MkPolarShim iJoinR2
        NegativeType -> MkPolarShim iMeetL2

iPolarPair ::
       forall (shim :: ShimKind Type) polarity a1 a2 b1 b2. (JoinMeetIsoShim shim, Is PolarityType polarity)
    => PolarShim shim polarity a1 a2
    -> PolarShim shim polarity b1 b2
    -> PolarShim shim polarity (JoinMeetType polarity a1 b1) (JoinMeetType polarity a2 b2)
iPolarPair =
    case polarityType @polarity of
        PositiveType -> \(MkPolarShim ar) (MkPolarShim br) -> MkPolarShim $ iJoinPair ar br
        NegativeType -> \(MkPolarShim ar) (MkPolarShim br) -> MkPolarShim $ iMeetPair ar br

iPolarSwap ::
       forall (shim :: ShimKind Type) polarity a b. (JoinMeetIsoShim shim, Is PolarityType polarity)
    => PolarShim shim polarity (JoinMeetType polarity a b) (JoinMeetType polarity b a)
iPolarSwap =
    case polarityType @polarity of
        PositiveType -> MkPolarShim iJoinSwap
        NegativeType -> MkPolarShim iMeetSwap

iPolarSwapL ::
       forall (shim :: ShimKind Type) polarity a b c. (JoinMeetIsoShim shim, Is PolarityType polarity)
    => PolarShim shim polarity (JoinMeetType polarity (JoinMeetType polarity a b) c) (JoinMeetType polarity a (JoinMeetType polarity b c))
iPolarSwapL =
    case polarityType @polarity of
        PositiveType -> MkPolarShim iJoinSwapL
        NegativeType -> MkPolarShim iMeetSwapR

iPolarSwapR ::
       forall (shim :: ShimKind Type) polarity a b c. (JoinMeetIsoShim shim, Is PolarityType polarity)
    => PolarShim shim polarity (JoinMeetType polarity a (JoinMeetType polarity b c)) (JoinMeetType polarity (JoinMeetType polarity a b) c)
iPolarSwapR =
    case polarityType @polarity of
        PositiveType -> MkPolarShim iJoinSwapR
        NegativeType -> MkPolarShim iMeetSwapL

polarLimit ::
       forall (shim :: ShimKind Type) polarity a. (Is PolarityType polarity, JoinMeetShim shim)
    => PolarShim shim polarity (LimitType polarity) a
polarLimit =
    case polarityType @polarity of
        PositiveType -> MkPolarShim initf
        NegativeType -> MkPolarShim termf

polar1 ::
       forall (shim :: ShimKind Type) polarity a b. (Is PolarityType polarity, JoinMeetShim shim)
    => PolarShim shim polarity a (JoinMeetType polarity a b)
polar1 =
    case polarityType @polarity of
        PositiveType -> MkPolarShim join1
        NegativeType -> MkPolarShim meet1

polar2 ::
       forall (shim :: ShimKind Type) polarity a b. (Is PolarityType polarity, JoinMeetShim shim)
    => PolarShim shim polarity b (JoinMeetType polarity a b)
polar2 =
    case polarityType @polarity of
        PositiveType -> MkPolarShim join2
        NegativeType -> MkPolarShim meet2

polarF ::
       forall (shim :: ShimKind Type) polarity a b r. (Is PolarityType polarity, JoinMeetShim shim)
    => PolarShim shim polarity a r
    -> PolarShim shim polarity b r
    -> PolarShim shim polarity (JoinMeetType polarity a b) r
polarF =
    case polarityType @polarity of
        PositiveType -> \(MkPolarShim ar) (MkPolarShim br) -> MkPolarShim $ joinf ar br
        NegativeType -> \(MkPolarShim ar) (MkPolarShim br) -> MkPolarShim $ meetf ar br
