module Data.Shim.Mono.Retract where

import Data.Shim.Mono.General
import Data.Shim.Polar
import Shapes

retractShim ::
       forall (shim :: ShimKind Type) a b. FunctionShim shim
    => shim a b
retractShim = functionToShim "BROKEN" $ \_ -> error "broken retraction"

isoRetractShim ::
       forall (shim :: ShimKind Type) a b. FunctionShim shim
    => shim a b
    -> Isomorphism shim a b
isoRetractShim conv = MkIsomorphism conv retractShim

retractJoin1 ::
       forall shim a b. (JoinMeetShim shim, FunctionShim shim)
    => shim (JoinType a b) a
retractJoin1 = joinf id retractShim

retractJoin2 ::
       forall shim a b. (JoinMeetShim shim, FunctionShim shim)
    => shim (JoinType a b) b
retractJoin2 = joinf retractShim id

retractMeet1 ::
       forall shim a b. (JoinMeetShim shim, FunctionShim shim)
    => shim a (MeetType a b)
retractMeet1 = meetf id retractShim

retractMeet2 ::
       forall shim a b. (JoinMeetShim shim, FunctionShim shim)
    => shim b (MeetType a b)
retractMeet2 = meetf retractShim id

isoRetractJoin1 ::
       forall shim a b. (JoinMeetShim shim, FunctionShim shim)
    => Isomorphism shim a (JoinType a b)
isoRetractJoin1 = MkIsomorphism join1 retractJoin1

isoRetractJoin2 ::
       forall shim a b. (JoinMeetShim shim, FunctionShim shim)
    => Isomorphism shim b (JoinType a b)
isoRetractJoin2 = MkIsomorphism join2 retractJoin2

isoRetractMeet1 ::
       forall shim a b. (JoinMeetShim shim, FunctionShim shim)
    => Isomorphism shim (MeetType a b) a
isoRetractMeet1 = MkIsomorphism meet1 retractMeet1

isoRetractMeet2 ::
       forall shim a b. (JoinMeetShim shim, FunctionShim shim)
    => Isomorphism shim (MeetType a b) b
isoRetractMeet2 = MkIsomorphism meet2 retractMeet2
