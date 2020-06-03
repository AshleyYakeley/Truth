module Pinafore.Language.Value.FiniteSetRef where

import Data.Shim
import Pinafore.Base
import Pinafore.Language.Shim
import Pinafore.Language.Value.OpenEntity
import Pinafore.Language.Value.Ref
import Pinafore.Language.Value.SetRef
import Shapes
import Truth.Core

data LangFiniteSetRef pq where
    MkLangFiniteSetRef
        :: Eq t => Range (PinaforePolyShim Type) t pq -> PinaforeRef (FiniteSetUpdate t) -> LangFiniteSetRef pq

unLangFiniteSetRef :: LangFiniteSetRef '( p, p) -> PinaforeRef (FiniteSetUpdate p)
unLangFiniteSetRef (MkLangFiniteSetRef tr lv) =
    eaMap (bijectionFiniteSetChangeLens $ isoMapCat fromEnhanced $ rangeBijection tr) lv

instance CatFunctor (CatRange (->)) (->) LangFiniteSetRef where
    cfmap f (MkLangFiniteSetRef r v) = MkLangFiniteSetRef (cfmap f r) v

instance HasVariance 'Rangevariance LangFiniteSetRef where
    varianceRepresentational = Nothing

langFiniteSetRefValue :: LangFiniteSetRef '( q, q) -> PinaforeRef (FiniteSetUpdate q)
langFiniteSetRefValue (MkLangFiniteSetRef tr lv) =
    eaMap (bijectionFiniteSetChangeLens (isoMapCat fromEnhanced $ rangeBijection tr)) lv

valueLangFiniteSetRef :: Eq q => PinaforeRef (FiniteSetUpdate q) -> LangFiniteSetRef '( q, q)
valueLangFiniteSetRef lv = MkLangFiniteSetRef identityRange lv

langFiniteSetRefMeetValue ::
       LangFiniteSetRef '( t, MeetType Entity t) -> PinaforeRef (FiniteSetUpdate (MeetType Entity t))
langFiniteSetRefMeetValue (MkLangFiniteSetRef tr lv) =
    langFiniteSetRefValue $ MkLangFiniteSetRef (contraMapRange meet2 tr) lv

meetValueLangFiniteSetRef ::
       PinaforeRef (FiniteSetUpdate (MeetType Entity t)) -> LangFiniteSetRef '( MeetType Entity t, t)
meetValueLangFiniteSetRef lv = MkLangFiniteSetRef (coMapRange meet2 identityRange) lv

langFiniteSetRefMeet ::
       forall t.
       LangFiniteSetRef '( t, MeetType Entity t)
    -> LangFiniteSetRef '( t, MeetType Entity t)
    -> LangFiniteSetRef '( MeetType Entity t, t)
langFiniteSetRefMeet seta setb =
    meetValueLangFiniteSetRef $
    eaMap (fromReadOnlyRejectingChangeLens . meetChangeLens) $
    eaPair (langFiniteSetRefMeetValue seta) (langFiniteSetRefMeetValue setb)

langFiniteSetRefJoin ::
       forall t.
       LangFiniteSetRef '( t, MeetType Entity t)
    -> LangFiniteSetRef '( t, MeetType Entity t)
    -> LangFiniteSetRef '( MeetType Entity t, t)
langFiniteSetRefJoin seta setb =
    meetValueLangFiniteSetRef $
    eaMap (fromReadOnlyRejectingChangeLens . joinChangeLens) $
    eaPair (langFiniteSetRefMeetValue seta) (langFiniteSetRefMeetValue setb)

langFiniteSetRefAdd :: LangFiniteSetRef '( p, q) -> p -> PinaforeAction ()
langFiniteSetRefAdd (MkLangFiniteSetRef tr set) p =
    pinaforeRefPushAction set $ pure $ KeyEditInsertReplace $ fromEnhanced (rangeContra tr) p

langFiniteSetRefAddNew :: LangFiniteSetRef '( NewEntity, TopType) -> PinaforeAction NewEntity
langFiniteSetRefAddNew set = do
    (MkNewEntity -> e) <- liftIO $ newKeyContainerItem @(FiniteSet Entity)
    langFiniteSetRefAdd set e
    return e

langFiniteSetRefRemove :: LangFiniteSetRef '( p, q) -> p -> PinaforeAction ()
langFiniteSetRefRemove (MkLangFiniteSetRef tr set) p =
    pinaforeRefPushAction set $ pure $ KeyEditDelete $ fromEnhanced (rangeContra tr) p

langFiniteSetRefRemoveAll :: LangFiniteSetRef '( BottomType, TopType) -> PinaforeAction ()
langFiniteSetRefRemoveAll (MkLangFiniteSetRef _ set) = pinaforeRefPushAction set $ pure KeyEditClear

langFiniteSetRefFunctionValue :: LangFiniteSetRef '( t, a) -> PinaforeROWRef (FiniteSet a)
langFiniteSetRefFunctionValue (MkLangFiniteSetRef tr set) =
    eaMapReadOnlyWhole (fmap $ fromEnhanced $ rangeCo tr) $ eaToReadOnlyWhole set

langFiniteSetRefMember :: forall a. LangFiniteSetRef '( a, TopType) -> a -> LangRef '( Bool, Bool)
langFiniteSetRefMember (MkLangFiniteSetRef tr set) val = let
    tval = fromEnhanced (rangeContra tr) val
    in pinaforeRefToRef $ eaMap (wholeChangeLens knowMaybeLens . finiteSetChangeLens tval) set

langFiniteSetRefSingle :: forall a. LangFiniteSetRef '( BottomType, MeetType Entity a) -> LangRef '( TopType, a)
langFiniteSetRefSingle set =
    pinaforeROWRefToRef $ eaMapReadOnlyWhole (fmap meet2 . maybeToKnow . getSingle) $ langFiniteSetRefFunctionValue set

langFiniteSetRefFunc :: forall a b. (FiniteSet a -> b) -> LangFiniteSetRef '( BottomType, a) -> LangRef '( TopType, b)
langFiniteSetRefFunc f set = pinaforeROWRefToRef $ eaMapReadOnlyWhole (Known . f) $ langFiniteSetRefFunctionValue set

langFiniteSetRefCartesianSum ::
       forall ap aq bp bq.
       LangFiniteSetRef '( ap, aq)
    -> LangFiniteSetRef '( bp, bq)
    -> LangFiniteSetRef '( Either ap bp, Either aq bq)
langFiniteSetRefCartesianSum (MkLangFiniteSetRef tra vala) (MkLangFiniteSetRef trb valb) =
    MkLangFiniteSetRef (eitherRange tra trb) $ eaMap finiteSetCartesianSumChangeLens $ eaPair vala valb

langFiniteSetRefCartesianProduct ::
       forall ap aq bp bq.
       LangFiniteSetRef '( ap, aq)
    -> LangFiniteSetRef '( bp, bq)
    -> LangFiniteSetRef '( (ap, bp), (aq, bq))
langFiniteSetRefCartesianProduct (MkLangFiniteSetRef tra vala) (MkLangFiniteSetRef trb valb) =
    MkLangFiniteSetRef (pairRange tra trb) $
    eaMap (fromReadOnlyRejectingChangeLens . finiteSetCartesianProductUpdateFunction) $ eaPair vala valb

langFiniteSetRefToSetRef :: forall p q. LangFiniteSetRef '( p, q) -> LangSetRef p
langFiniteSetRefToSetRef (MkLangFiniteSetRef tr sval) =
    contramap (fromEnhanced $ rangeContra tr) $ MkLangSetRef (==) $ eaMap finiteSetFunctionChangeLens sval

langFiniteSetRefSetIntersect :: forall p q. LangFiniteSetRef '( p, q) -> LangSetRef q -> LangFiniteSetRef '( p, q)
langFiniteSetRefSetIntersect (MkLangFiniteSetRef tr fsetval) fsetref = let
    MkLangSetRef _ setval = contramap (fromEnhanced $ rangeCo tr) fsetref
    in MkLangFiniteSetRef tr $
       eaMap (fromReadOnlyRejectingChangeLens . filterFiniteSetUpdateFunction) $ eaPair fsetval setval

langFiniteSetRefSetDifference :: forall p q. LangFiniteSetRef '( p, q) -> LangSetRef q -> LangFiniteSetRef '( p, q)
langFiniteSetRefSetDifference a b = langFiniteSetRefSetIntersect a $ langSetRefComplement b

wholeListFiniteSetChangeLens :: Eq a => ChangeLens (WholeUpdate [a]) (FiniteSetUpdate a)
wholeListFiniteSetChangeLens = convertChangeLens . bijectionWholeChangeLens isoCoerce

langListRefToFiniteSetRef :: forall a. LangRef '( [a], [MeetType Entity a]) -> LangFiniteSetRef '( MeetType Entity a, a)
langListRefToFiniteSetRef ref =
    meetValueLangFiniteSetRef $
    eaMap
        (wholeListFiniteSetChangeLens .
         unknownValueChangeLens [] . biSingleChangeLens . mapBiWholeChangeLens (fmap $ fmap meet2) id) $
    langRefToBiWholeRef ref
