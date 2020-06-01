module Data.Shim.PolarJoinMeet where

import Data.Shim.JoinMeet
import Data.Shim.PolarMap
import Data.Shim.Polarity
import Data.Shim.PolyMap
import Shapes

type family LimitType polarity :: Type where
    LimitType 'Positive = BottomType
    LimitType 'Negative = TopType

type family JoinMeetType polarity :: Type -> Type -> Type where
    JoinMeetType 'Positive = JoinType
    JoinMeetType 'Negative = MeetType

unjoinmeet1 ::
       forall pol map t. (JoinMeetCategory map, Is PolarityType pol)
    => map (JoinMeetType pol t (LimitType pol)) t
unjoinmeet1 =
    case polarityType @pol of
        PositiveType -> unjoin1
        NegativeType -> meet1

polarLimit ::
       forall (map :: MapKind Type) polarity a. (Is PolarityType polarity, JoinMeetCategory map)
    => PolarMap map polarity (LimitType polarity) a
polarLimit =
    case polarityType @polarity of
        PositiveType -> MkPolarMap initf
        NegativeType -> MkPolarMap termf

polar1 ::
       forall (map :: MapKind Type) polarity a b. (Is PolarityType polarity, JoinMeetCategory map)
    => PolarMap map polarity a (JoinMeetType polarity a b)
polar1 =
    case polarityType @polarity of
        PositiveType -> MkPolarMap join1
        NegativeType -> MkPolarMap meet1

polar2 ::
       forall (map :: MapKind Type) polarity a b. (Is PolarityType polarity, JoinMeetCategory map)
    => PolarMap map polarity b (JoinMeetType polarity a b)
polar2 =
    case polarityType @polarity of
        PositiveType -> MkPolarMap join2
        NegativeType -> MkPolarMap meet2

polarF ::
       forall (map :: MapKind Type) polarity a b r. (Is PolarityType polarity, JoinMeetCategory map)
    => PolarMap map polarity a r
    -> PolarMap map polarity b r
    -> PolarMap map polarity (JoinMeetType polarity a b) r
polarF =
    case polarityType @polarity of
        PositiveType -> \(MkPolarMap ar) (MkPolarMap br) -> MkPolarMap $ joinf ar br
        NegativeType -> \(MkPolarMap ar) (MkPolarMap br) -> MkPolarMap $ meetf ar br

polarUn1 ::
       forall (map :: MapKind Type) polarity a. (Is PolarityType polarity, JoinMeetCategory map)
    => PolarMap map polarity (JoinMeetType polarity a (LimitType polarity)) a
polarUn1 = polarF cid polarLimit

polarUn2 ::
       forall (map :: MapKind Type) polarity a. (Is PolarityType polarity, JoinMeetCategory map)
    => PolarMap map polarity (JoinMeetType polarity (LimitType polarity) a) a
polarUn2 = polarF polarLimit cid

polarBimap ::
       forall (map :: MapKind Type) polarity a1 a2 b1 b2. (Is PolarityType polarity, JoinMeetCategory map)
    => PolarMap map polarity a1 a2
    -> PolarMap map polarity b1 b2
    -> PolarMap map polarity (JoinMeetType polarity a1 b1) (JoinMeetType polarity a2 b2)
polarBimap aa bb = polarF (polar1 <.> aa) (polar2 <.> bb)

polarSwapRight ::
       forall (map :: MapKind Type) polarity a b c. (Is PolarityType polarity, JoinMeetCategory map)
    => PolarMap map polarity (JoinMeetType polarity a (JoinMeetType polarity b c)) (JoinMeetType polarity (JoinMeetType polarity a b) c)
polarSwapRight = polarF (polar1 <.> polar1) (polarF (polar1 <.> polar2) polar2)
