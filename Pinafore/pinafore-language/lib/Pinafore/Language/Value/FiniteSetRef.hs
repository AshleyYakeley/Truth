module Pinafore.Language.Value.FiniteSetRef where

import Changes.Core
import Data.Shim
import Pinafore.Base
import Pinafore.Language.Shim
import Pinafore.Language.Value.SetRef
import Pinafore.Language.Value.WholeRef
import Shapes

data LangFiniteSetRef pq where
    MkLangFiniteSetRef
        :: Eq t => Range (PinaforePolyShim Type) t pq -> WModel (FiniteSetUpdate t) -> LangFiniteSetRef pq

unLangFiniteSetRef :: LangFiniteSetRef '( p, p) -> WModel (FiniteSetUpdate p)
unLangFiniteSetRef (MkLangFiniteSetRef tr lv) =
    eaMap (bijectionFiniteSetChangeLens $ isoMapCat shimToFunction $ rangeBijection tr) lv

instance CatFunctor (CatRange (->)) (->) LangFiniteSetRef where
    cfmap f (MkLangFiniteSetRef r v) = MkLangFiniteSetRef (cfmap f r) v

instance MaybeRepresentational LangFiniteSetRef where
    maybeRepresentational = Nothing

instance HasCCRVariance 'RangeCCRVariance LangFiniteSetRef

langFiniteSetRefValue :: LangFiniteSetRef '( q, q) -> WModel (FiniteSetUpdate q)
langFiniteSetRefValue (MkLangFiniteSetRef tr lv) =
    eaMap (bijectionFiniteSetChangeLens (isoMapCat shimToFunction $ rangeBijection tr)) lv

valueLangFiniteSetRef :: Eq q => WModel (FiniteSetUpdate q) -> LangFiniteSetRef '( q, q)
valueLangFiniteSetRef lv = MkLangFiniteSetRef identityRange lv

langFiniteSetMaybeMap ::
       forall ap aq b. (aq -> Maybe b) -> LangFiniteSetRef '( ap, aq) -> LangFiniteSetRef '( MeetType ap b, b)
langFiniteSetMaybeMap f (MkLangFiniteSetRef (tr :: _ t _) lv) = let
    tr' :: Range (PinaforePolyShim Type) (MeetType t b) '( MeetType ap b, b)
    tr' = MkRange (iMeetPair (rangeContra tr) id) meet2
    amb :: t -> Maybe (MeetType t b)
    amb t = do
        b <- f $ shimToFunction (rangeCo tr) t
        return $ BothMeetType t b
    in MkLangFiniteSetRef tr' $ eaMap (mapMaybeFiniteSetChangeLens amb meet1) lv

langFiniteSetRefMeetValue :: LangFiniteSetRef '( t, MeetType Entity t) -> WModel (FiniteSetUpdate (MeetType Entity t))
langFiniteSetRefMeetValue (MkLangFiniteSetRef tr lv) =
    langFiniteSetRefValue $ MkLangFiniteSetRef (contraMapRange meet2 tr) lv

meetValueLangFiniteSetRef :: WModel (FiniteSetUpdate (MeetType Entity t)) -> LangFiniteSetRef '( MeetType Entity t, t)
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
    pinaforeRefPush set $ pure $ KeyEditInsertReplace $ shimToFunction (rangeContra tr) p

langFiniteSetRefRemove :: LangFiniteSetRef '( p, q) -> p -> PinaforeAction ()
langFiniteSetRefRemove (MkLangFiniteSetRef tr set) p =
    pinaforeRefPush set $ pure $ KeyEditDelete $ shimToFunction (rangeContra tr) p

langFiniteSetRefRemoveAll :: LangFiniteSetRef '( BottomType, TopType) -> PinaforeAction ()
langFiniteSetRefRemoveAll (MkLangFiniteSetRef _ set) = pinaforeRefPush set $ pure KeyEditClear

langFiniteSetRefFunctionValue :: LangFiniteSetRef '( t, a) -> PinaforeROWRef (FiniteSet a)
langFiniteSetRefFunctionValue (MkLangFiniteSetRef tr set) =
    eaMapReadOnlyWhole (fmap $ shimToFunction $ rangeCo tr) $ eaToReadOnlyWhole set

langFiniteSetRefMember :: forall a. LangFiniteSetRef '( a, TopType) -> a -> LangWholeRef '( Bool, Bool)
langFiniteSetRefMember (MkLangFiniteSetRef tr set) val = let
    tval = shimToFunction (rangeContra tr) val
    in pinaforeRefToWholeRef $ eaMap (wholeChangeLens knowMaybeLens . finiteSetChangeLens tval) set

langFiniteSetRefSingle :: forall a. LangFiniteSetRef '( BottomType, MeetType Entity a) -> LangWholeRef '( TopType, a)
langFiniteSetRefSingle set =
    pinaforeROWRefToWholeRef $
    eaMapReadOnlyWhole (fmap meet2 . maybeToKnow . getSingle) $ langFiniteSetRefFunctionValue set

langFiniteSetRefFunc ::
       forall a b. (FiniteSet a -> b) -> LangFiniteSetRef '( BottomType, a) -> LangWholeRef '( TopType, b)
langFiniteSetRefFunc f set =
    pinaforeROWRefToWholeRef $ eaMapReadOnlyWhole (Known . f) $ langFiniteSetRefFunctionValue set

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
    contramap (shimToFunction $ rangeContra tr) $ MkLangSetRef (==) $ eaMap finiteSetFunctionChangeLens sval

langFiniteSetRefSetIntersect :: forall p q. LangFiniteSetRef '( p, q) -> LangSetRef q -> LangFiniteSetRef '( p, q)
langFiniteSetRefSetIntersect (MkLangFiniteSetRef tr fsetval) fsetref = let
    MkLangSetRef _ setval = contramap (shimToFunction $ rangeCo tr) fsetref
    in MkLangFiniteSetRef tr $
       eaMap (fromReadOnlyRejectingChangeLens . filterFiniteSetUpdateFunction) $ eaPair fsetval setval

langFiniteSetRefSetDifference :: forall p q. LangFiniteSetRef '( p, q) -> LangSetRef q -> LangFiniteSetRef '( p, q)
langFiniteSetRefSetDifference a b = langFiniteSetRefSetIntersect a $ langSetRefComplement b

wholeListFiniteSetChangeLens :: Eq a => ChangeLens (WholeUpdate [a]) (FiniteSetUpdate a)
wholeListFiniteSetChangeLens = convertChangeLens . bijectionWholeChangeLens coerceIsomorphism

langListRefToFiniteSetRef ::
       forall a. LangWholeRef '( [a], [MeetType Entity a]) -> LangFiniteSetRef '( MeetType Entity a, a)
langListRefToFiniteSetRef ref =
    meetValueLangFiniteSetRef $
    eaMap
        (wholeListFiniteSetChangeLens .
         unknownValueChangeLens [] . biSingleChangeLens . mapBiWholeChangeLens (fmap $ fmap meet2) id) $
    langWholeRefToBiWholeRef ref
