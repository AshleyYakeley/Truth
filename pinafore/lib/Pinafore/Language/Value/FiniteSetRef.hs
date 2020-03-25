module Pinafore.Language.Value.FiniteSetRef where

import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.OpenEntity
import Pinafore.Language.Value.Ref
import Pinafore.Language.Value.SetRef
import Shapes
import Truth.Core

data PinaforeFiniteSetRef pq where
    MkPinaforeFiniteSetRef :: Eq t => Range JMShim t pq -> PinaforeValue (FiniteSetUpdate t) -> PinaforeFiniteSetRef pq

unPinaforeFiniteSetRef :: PinaforeFiniteSetRef '( p, p) -> PinaforeValue (FiniteSetUpdate p)
unPinaforeFiniteSetRef (MkPinaforeFiniteSetRef tr lv) =
    eaMap (bijectionFiniteSetChangeLens $ isoMapCat fromEnhanced $ rangeBijection tr) lv

instance CatFunctor (CatRange (->)) (->) PinaforeFiniteSetRef where
    cfmap f (MkPinaforeFiniteSetRef r v) = MkPinaforeFiniteSetRef (cfmap f r) v

instance HasVariance 'Rangevariance PinaforeFiniteSetRef where
    varianceRepresentational = Nothing

pinaforeFiniteSetRefValue :: PinaforeFiniteSetRef '( q, q) -> PinaforeValue (FiniteSetUpdate q)
pinaforeFiniteSetRefValue (MkPinaforeFiniteSetRef tr lv) =
    eaMap (bijectionFiniteSetChangeLens (isoMapCat fromEnhanced $ rangeBijection tr)) lv

valuePinaforeFiniteSetRef :: Eq q => PinaforeValue (FiniteSetUpdate q) -> PinaforeFiniteSetRef '( q, q)
valuePinaforeFiniteSetRef lv = MkPinaforeFiniteSetRef identityRange lv

pinaforeFiniteSetRefMeetValue ::
       PinaforeFiniteSetRef '( t, MeetType Entity t) -> PinaforeValue (FiniteSetUpdate (MeetType Entity t))
pinaforeFiniteSetRefMeetValue (MkPinaforeFiniteSetRef tr lv) =
    pinaforeFiniteSetRefValue $ MkPinaforeFiniteSetRef (contraMapRange meet2 tr) lv

meetValuePinaforeFiniteSetRef ::
       PinaforeValue (FiniteSetUpdate (MeetType Entity t)) -> PinaforeFiniteSetRef '( MeetType Entity t, t)
meetValuePinaforeFiniteSetRef lv = MkPinaforeFiniteSetRef (coMapRange meet2 identityRange) lv

pinaforeFiniteSetRefMeet ::
       forall t.
       PinaforeFiniteSetRef '( t, MeetType Entity t)
    -> PinaforeFiniteSetRef '( t, MeetType Entity t)
    -> PinaforeFiniteSetRef '( MeetType Entity t, t)
pinaforeFiniteSetRefMeet seta setb =
    meetValuePinaforeFiniteSetRef $
    eaMap (fromReadOnlyRejectingChangeLens . meetChangeLens) $
    eaPair (pinaforeFiniteSetRefMeetValue seta) (pinaforeFiniteSetRefMeetValue setb)

pinaforeFiniteSetRefJoin ::
       forall t.
       PinaforeFiniteSetRef '( t, MeetType Entity t)
    -> PinaforeFiniteSetRef '( t, MeetType Entity t)
    -> PinaforeFiniteSetRef '( MeetType Entity t, t)
pinaforeFiniteSetRefJoin seta setb =
    meetValuePinaforeFiniteSetRef $
    eaMap (fromReadOnlyRejectingChangeLens . joinChangeLens) $
    eaPair (pinaforeFiniteSetRefMeetValue seta) (pinaforeFiniteSetRefMeetValue setb)

pinaforeFiniteSetRefAdd :: PinaforeFiniteSetRef '( p, q) -> p -> PinaforeAction ()
pinaforeFiniteSetRefAdd (MkPinaforeFiniteSetRef tr set) p =
    pinaforeValuePushAction set $ pure $ KeyEditInsertReplace $ fromEnhanced (rangeContra tr) p

pinaforeFiniteSetRefAddNew :: PinaforeFiniteSetRef '( NewEntity, TopType) -> PinaforeAction NewEntity
pinaforeFiniteSetRefAddNew set = do
    (MkNewEntity -> e) <- liftIO $ newKeyContainerItem @(FiniteSet Entity)
    pinaforeFiniteSetRefAdd set e
    return e

pinaforeFiniteSetRefRemove :: PinaforeFiniteSetRef '( p, q) -> p -> PinaforeAction ()
pinaforeFiniteSetRefRemove (MkPinaforeFiniteSetRef tr set) p =
    pinaforeValuePushAction set $ pure $ KeyEditDelete $ fromEnhanced (rangeContra tr) p

pinaforeFiniteSetRefRemoveAll :: PinaforeFiniteSetRef '( BottomType, TopType) -> PinaforeAction ()
pinaforeFiniteSetRefRemoveAll (MkPinaforeFiniteSetRef _ set) = pinaforeValuePushAction set $ pure KeyEditClear

pinaforeFiniteSetRefFunctionValue :: PinaforeFiniteSetRef '( t, a) -> PinaforeReadOnlyValue (FiniteSet a)
pinaforeFiniteSetRefFunctionValue (MkPinaforeFiniteSetRef tr set) =
    eaMapReadOnlyWhole (fmap $ fromEnhanced $ rangeCo tr) $ eaToReadOnlyWhole set

pinaforeFiniteSetRefMember :: forall a. PinaforeFiniteSetRef '( a, TopType) -> a -> PinaforeRef '( Bool, Bool)
pinaforeFiniteSetRefMember (MkPinaforeFiniteSetRef tr set) val = let
    tval = fromEnhanced (rangeContra tr) val
    in MutablePinaforeRef identityRange $ eaMap (wholeChangeLens knowMaybeLens . finiteSetChangeLens tval) set

pinaforeFiniteSetRefSingle ::
       forall a. PinaforeFiniteSetRef '( BottomType, MeetType Entity a) -> PinaforeRef '( TopType, a)
pinaforeFiniteSetRefSingle set =
    pinaforeReadOnlyValueToRef $
    eaMapReadOnlyWhole (fmap meet2 . maybeToKnow . getSingle) $ pinaforeFiniteSetRefFunctionValue set

pinaforeFiniteSetRefFunc ::
       forall a b. (FiniteSet a -> b) -> PinaforeFiniteSetRef '( BottomType, a) -> PinaforeRef '( TopType, b)
pinaforeFiniteSetRefFunc f set =
    pinaforeReadOnlyValueToRef $ eaMapReadOnlyWhole (Known . f) $ pinaforeFiniteSetRefFunctionValue set

pinaforeFiniteSetRefCartesianSum ::
       forall ap aq bp bq.
       PinaforeFiniteSetRef '( ap, aq)
    -> PinaforeFiniteSetRef '( bp, bq)
    -> PinaforeFiniteSetRef '( Either ap bp, Either aq bq)
pinaforeFiniteSetRefCartesianSum (MkPinaforeFiniteSetRef tra vala) (MkPinaforeFiniteSetRef trb valb) =
    MkPinaforeFiniteSetRef (eitherRange tra trb) $ eaMap finiteSetCartesianSumChangeLens $ eaPair vala valb

pinaforeFiniteSetRefCartesianProduct ::
       forall ap aq bp bq.
       PinaforeFiniteSetRef '( ap, aq)
    -> PinaforeFiniteSetRef '( bp, bq)
    -> PinaforeFiniteSetRef '( (ap, bp), (aq, bq))
pinaforeFiniteSetRefCartesianProduct (MkPinaforeFiniteSetRef tra vala) (MkPinaforeFiniteSetRef trb valb) =
    MkPinaforeFiniteSetRef (pairRange tra trb) $
    eaMap (fromReadOnlyRejectingChangeLens . finiteSetCartesianProductUpdateFunction) $ eaPair vala valb

pinaforeFiniteSetRefToSetRef :: forall p q. PinaforeFiniteSetRef '( p, q) -> PinaforeSetRef p
pinaforeFiniteSetRefToSetRef (MkPinaforeFiniteSetRef tr sval) =
    contramap (fromEnhanced $ rangeContra tr) $ MkPinaforeSetRef (==) $ eaMap finiteSetFunctionChangeLens sval

pinaforeFiniteSetRefSetIntersect ::
       forall p q. PinaforeFiniteSetRef '( p, q) -> PinaforeSetRef q -> PinaforeFiniteSetRef '( p, q)
pinaforeFiniteSetRefSetIntersect (MkPinaforeFiniteSetRef tr fsetval) fsetref = let
    MkPinaforeSetRef _ setval = contramap (fromEnhanced $ rangeCo tr) fsetref
    in MkPinaforeFiniteSetRef tr $
       eaMap (fromReadOnlyRejectingChangeLens . filterFiniteSetUpdateFunction) $ eaPair fsetval setval

pinaforeFiniteSetRefSetDifference ::
       forall p q. PinaforeFiniteSetRef '( p, q) -> PinaforeSetRef q -> PinaforeFiniteSetRef '( p, q)
pinaforeFiniteSetRefSetDifference a b = pinaforeFiniteSetRefSetIntersect a $ pinaforeSetRefComplement b
