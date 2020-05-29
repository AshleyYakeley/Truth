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

unjoinmeet1 ::
       forall pol cat t. (JoinMeetCategory cat, Is PolarityType pol)
    => cat (JoinMeetType pol t (LimitType pol)) t
unjoinmeet1 =
    case polarityType @pol of
        PositiveType -> unjoin1
        NegativeType -> meet1

polarLimit ::
       forall polarity cat a. (Is PolarityType polarity, JoinMeetCategory cat)
    => PolarMap cat polarity (LimitType polarity) a
polarLimit =
    case polarityType @polarity of
        PositiveType -> MkPolarMap initf
        NegativeType -> MkPolarMap termf

polar1 ::
       forall polarity cat a b. (Is PolarityType polarity, JoinMeetCategory cat)
    => PolarMap cat polarity a (JoinMeetType polarity a b)
polar1 =
    case polarityType @polarity of
        PositiveType -> MkPolarMap join1
        NegativeType -> MkPolarMap meet1

polar2 ::
       forall polarity cat a b. (Is PolarityType polarity, JoinMeetCategory cat)
    => PolarMap cat polarity b (JoinMeetType polarity a b)
polar2 =
    case polarityType @polarity of
        PositiveType -> MkPolarMap join2
        NegativeType -> MkPolarMap meet2

polarF ::
       forall polarity cat a b r. (Is PolarityType polarity, JoinMeetCategory cat)
    => PolarMap cat polarity a r
    -> PolarMap cat polarity b r
    -> PolarMap cat polarity (JoinMeetType polarity a b) r
polarF =
    case polarityType @polarity of
        PositiveType -> \(MkPolarMap ar) (MkPolarMap br) -> MkPolarMap $ joinf ar br
        NegativeType -> \(MkPolarMap ar) (MkPolarMap br) -> MkPolarMap $ meetf ar br

polarUn1 ::
       forall polarity cat a. (Is PolarityType polarity, JoinMeetCategory cat)
    => PolarMap cat polarity (JoinMeetType polarity a (LimitType polarity)) a
polarUn1 = polarF cid polarLimit

polarUn2 ::
       forall polarity cat a. (Is PolarityType polarity, JoinMeetCategory cat)
    => PolarMap cat polarity (JoinMeetType polarity (LimitType polarity) a) a
polarUn2 = polarF polarLimit cid

polarBimap ::
       forall polarity cat a1 a2 b1 b2. (Is PolarityType polarity, JoinMeetCategory cat)
    => PolarMap cat polarity a1 a2
    -> PolarMap cat polarity b1 b2
    -> PolarMap cat polarity (JoinMeetType polarity a1 b1) (JoinMeetType polarity a2 b2)
polarBimap aa bb = polarF (polar1 <.> aa) (polar2 <.> bb)

polarSwapRight ::
       forall polarity cat a b c. (Is PolarityType polarity, JoinMeetCategory cat)
    => PolarMap cat polarity (JoinMeetType polarity a (JoinMeetType polarity b c)) (JoinMeetType polarity (JoinMeetType polarity a b) c)
polarSwapRight = polarF (polar1 <.> polar1) (polarF (polar1 <.> polar2) polar2)
