module Pinafore.Language.Set where

import Language.Expression.Dolan
import Pinafore.Action
import Pinafore.Know
import Pinafore.Language.Entity
import Pinafore.Language.Order
import Pinafore.Language.Reference
import Pinafore.Morphism
import Pinafore.Table (Point)
import Shapes
import Truth.Core

data PinaforeSet baseedit pq =
    forall t. Eq t =>
                  MkPinaforeSet (TypeRange t pq)
                                (PinaforeLensValue baseedit (FiniteSetEdit t))

unPinaforeSet :: PinaforeSet baseedit '( p, p) -> PinaforeLensValue baseedit (FiniteSetEdit p)
unPinaforeSet (MkPinaforeSet tr lv) = (bijectionFiniteSetEditLens $ typeRangeBijection tr) . lv

instance IsoMapTypeRange (PinaforeSet baseedit)

instance MapTypeRange (PinaforeSet baseedit) where
    mapTypeRange f (MkPinaforeSet r s) = MkPinaforeSet (mapTypeRange f r) s

instance HasDolanVary '[ 'Rangevariance] (PinaforeSet baseedit) where
    dolanVary =
        ConsDolanKindVary (mkRangevary $ \mapr (MkPinaforeSet range lval) -> MkPinaforeSet (mapr range) lval) $
        NilDolanKindVary

pinaforeSetValue :: PinaforeSet baseedit '( q, q) -> PinaforeLensValue baseedit (FiniteSetEdit q)
pinaforeSetValue (MkPinaforeSet tr lv) = bijectionFiniteSetEditLens (typeRangeBijection tr) . lv

valuePinaforeSet :: Eq q => PinaforeLensValue baseedit (FiniteSetEdit q) -> PinaforeSet baseedit '( q, q)
valuePinaforeSet lv = MkPinaforeSet identityTypeRange lv

pinaforeSetMeetValue ::
       PinaforeSet baseedit '( t, MeetType Entity t) -> PinaforeLensValue baseedit (FiniteSetEdit (MeetType Entity t))
pinaforeSetMeetValue (MkPinaforeSet tr lv) = pinaforeSetValue $ MkPinaforeSet (unifyTypeRange2 meet2 tr) lv

meetValuePinaforeSet ::
       PinaforeLensValue baseedit (FiniteSetEdit (MeetType Entity t)) -> PinaforeSet baseedit '( MeetType Entity t, t)
meetValuePinaforeSet lv = MkPinaforeSet (unUnifyTypeRange1 meet2 identityTypeRange) lv

pinaforeSetMeet ::
       PinaforeSet baseedit '( t, MeetType Entity t)
    -> PinaforeSet baseedit '( t, MeetType Entity t)
    -> PinaforeSet baseedit '( MeetType Entity t, t)
pinaforeSetMeet seta setb =
    meetValuePinaforeSet $
    readOnlyEditLens meetEditFunction . pairJoinEditLenses (pinaforeSetMeetValue seta) (pinaforeSetMeetValue setb)

pinaforeSetJoin ::
       PinaforeSet baseedit '( t, MeetType Entity t)
    -> PinaforeSet baseedit '( t, MeetType Entity t)
    -> PinaforeSet baseedit '( MeetType Entity t, t)
pinaforeSetJoin seta setb =
    meetValuePinaforeSet $
    readOnlyEditLens joinEditFunction . pairJoinEditLenses (pinaforeSetMeetValue seta) (pinaforeSetMeetValue setb)

pinaforeSetAdd :: PinaforeSet baseedit '( p, q) -> p -> PinaforeAction baseedit
pinaforeSetAdd (MkPinaforeSet tr set) p =
    pinaforeLiftView $
    viewMapEdit set $ viewObjectPushEdit $ \_ push -> push [KeyInsertReplaceItem $ typeRangeContra tr p]

pinaforeSetAddNew :: PinaforeSet baseedit '( Point, TopType) -> PinaforeActionM baseedit Point
pinaforeSetAddNew set = do
    point <- pinaforeGeneratePoint
    pinaforeSetAdd set point
    return point

pinaforeSetRemove :: PinaforeSet baseedit '( p, q) -> p -> PinaforeAction baseedit
pinaforeSetRemove (MkPinaforeSet tr set) p =
    pinaforeLiftView $ viewMapEdit set $ viewObjectPushEdit $ \_ push -> push [KeyDeleteItem $ typeRangeContra tr p]

pinaforeSetRemoveAll :: PinaforeSet baseedit '( BottomType, TopType) -> PinaforeAction baseedit
pinaforeSetRemoveAll (MkPinaforeSet _ set) =
    pinaforeLiftView $ viewMapEdit set $ viewObjectPushEdit $ \_ push -> push [KeyClear]

pinaforeSetFunctionValue :: PinaforeSet baseedit '( t, a) -> PinaforeFunctionValue baseedit (FiniteSet a)
pinaforeSetFunctionValue (MkPinaforeSet tr set) = funcEditFunction (fmap $ typeRangeCo tr) . lensFunctionValue set

pinaforeSetContains ::
       PinaforeSet baseedit '( BottomType, Entity) -> PinaforeReference baseedit '( BottomType, Entity -> Bool)
pinaforeSetContains set =
    pinaforeFunctionToReference $ funcEditFunction (\s -> Known $ \p -> member p s) . pinaforeSetFunctionValue set

pinaforeSetGetOrdered ::
       forall baseedit a.
       PinaforeOrder baseedit a
    -> PinaforeSet baseedit '( BottomType, a)
    -> PinaforeReference baseedit '( BottomType, [a])
pinaforeSetGetOrdered order set =
    pinaforeFunctionToReference $ funcEditFunction Known . (qOrderSet order $ pinaforeSetFunctionValue set)

pinaforeSetSingle ::
       forall baseedit a.
       PinaforeSet baseedit '( BottomType, MeetType Entity a)
    -> PinaforeReference baseedit '( BottomType, a)
pinaforeSetSingle set =
    pinaforeFunctionToReference $ funcEditFunction (fmap meet2 . maybeToKnow . getSingle) . pinaforeSetFunctionValue set

pinaforeSetFunc ::
       forall baseedit a b.
       (FiniteSet a -> b)
    -> PinaforeSet baseedit '( BottomType, a)
    -> PinaforeReference baseedit '( BottomType, b)
pinaforeSetFunc f set = pinaforeFunctionToReference $ funcEditFunction (Known . f) . pinaforeSetFunctionValue set
