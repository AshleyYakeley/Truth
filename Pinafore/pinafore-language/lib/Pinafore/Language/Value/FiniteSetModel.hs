module Pinafore.Language.Value.FiniteSetModel where

import Import
import Pinafore.Language.Type.Ground
import Pinafore.Language.Value.Model
import Pinafore.Language.Value.SetModel
import Pinafore.Language.Value.WholeModel

data LangFiniteSetModel pq where
    MkLangFiniteSetModel :: Equivalence t -> QRange t pq -> WModel (FiniteSetUpdate t) -> LangFiniteSetModel pq

instance CatFunctor (CatRange (->)) (->) LangFiniteSetModel where
    cfmap f (MkLangFiniteSetModel eqv r v) = MkLangFiniteSetModel eqv (cfmap f r) v

instance MaybeRepresentational LangFiniteSetModel where
    maybeRepresentational = Nothing

instance HasCCRVariance 'RangeCCRVariance LangFiniteSetModel

instance IsInvertibleModel (LangFiniteSetModel pq) where
    invertibleModelLens f (MkLangFiniteSetModel eqv tr model) =
        fmap (MkLangFiniteSetModel eqv tr) $ giveConstraint eqv $ wInvertibleModelLens f model

newMemFiniteSetModel :: forall a. Action (LangFiniteSetModel '( MeetType Entity a, a))
newMemFiniteSetModel = do
    r <- liftIO $ makeMemoryReference mempty $ \_ -> True
    model <- actionLiftLifecycle $ makeReflectingModel $ convertReference r
    return $ meetValueLangFiniteSetModel $ MkWModel model

langFiniteSetModelValue :: LangFiniteSetModel '( q, q) -> WModel (FiniteSetUpdate q)
langFiniteSetModelValue (MkLangFiniteSetModel _ tr lv) =
    eaMap (bijectionFiniteSetChangeLens (isoMapCat shimToFunction $ rangeBijection tr)) lv

valueLangFiniteSetModel :: Equivalence q -> WModel (FiniteSetUpdate q) -> LangFiniteSetModel '( q, q)
valueLangFiniteSetModel eqv lv = MkLangFiniteSetModel eqv identityRange lv

langFiniteSetFilter :: forall ap aq. (aq -> Bool) -> LangFiniteSetModel '( ap, aq) -> LangFiniteSetModel '( ap, aq)
langFiniteSetFilter f (MkLangFiniteSetModel eqv tr lv) =
    MkLangFiniteSetModel eqv tr $ eaMap (filterFiniteSetChangeLens $ f . shimToFunction (rangeCo tr)) lv

langFiniteSetMaybeMap ::
       forall ap aq b. (aq -> Maybe b) -> LangFiniteSetModel '( ap, aq) -> LangFiniteSetModel '( MeetType ap b, b)
langFiniteSetMaybeMap f (MkLangFiniteSetModel eqv (tr :: _ t _) lv) = let
    tr' :: QRange (MeetType t b) '( MeetType ap b, b)
    tr' = MkRange (iMeetPair (rangeContra tr) id) meet2
    amb :: t -> Maybe (MeetType t b)
    amb t = do
        b <- f $ shimToFunction (rangeCo tr) t
        return $ BothMeetType t b
    in MkLangFiniteSetModel (contramap meet1 eqv) tr' $ eaMap (injectiveMapFiniteSetChangeLens $ MkCodec amb meet1) lv

langFiniteSetCollect ::
       forall ap aq b.
       ImmutableWholeModel (aq -> Maybe b)
    -> LangFiniteSetModel '( ap, aq)
    -> LangFiniteSetModel '( MeetType ap b, b)
langFiniteSetCollect (MkImmutableWholeModel mf) (MkLangFiniteSetModel eqv (tr :: _ t _) lv) = let
    tr' :: QRange (MeetType t b) '( MeetType ap b, b)
    tr' = MkRange (iMeetPair (rangeContra tr) id) meet2
    amb :: Know (aq -> Maybe b) -> (t -> Maybe (MeetType t b))
    amb Unknown _ = Nothing
    amb (Known f) t = do
        b <- f $ shimToFunction (rangeCo tr) t
        return $ BothMeetType t b
    mf' = eaMapReadOnlyWhole amb mf
    in MkLangFiniteSetModel (contramap meet1 eqv) tr' $ eaMap (collectFiniteSetChangeLens meet1) $ eaPair mf' lv

langFiniteSetModelMeetValue ::
       LangFiniteSetModel '( t, MeetType Entity t) -> WModel (FiniteSetUpdate (MeetType Entity t))
langFiniteSetModelMeetValue (MkLangFiniteSetModel eqv tr lv) =
    langFiniteSetModelValue $ MkLangFiniteSetModel eqv (contraMapRange meet2 tr) lv

meetValueLangFiniteSetModel ::
       WModel (FiniteSetUpdate (MeetType Entity t)) -> LangFiniteSetModel '( MeetType Entity t, t)
meetValueLangFiniteSetModel lv = MkLangFiniteSetModel eqEquivalence (coMapRange meet2 identityRange) lv

langFiniteSetModelMeet ::
       forall t.
       LangFiniteSetModel '( t, MeetType Entity t)
    -> LangFiniteSetModel '( t, MeetType Entity t)
    -> LangFiniteSetModel '( MeetType Entity t, t)
langFiniteSetModelMeet seta setb =
    meetValueLangFiniteSetModel $
    eaMap (fromReadOnlyRejectingChangeLens . meetChangeLens) $
    eaPair (langFiniteSetModelMeetValue seta) (langFiniteSetModelMeetValue setb)

langFiniteSetModelJoin ::
       forall t.
       LangFiniteSetModel '( t, MeetType Entity t)
    -> LangFiniteSetModel '( t, MeetType Entity t)
    -> LangFiniteSetModel '( MeetType Entity t, t)
langFiniteSetModelJoin seta setb =
    meetValueLangFiniteSetModel $
    eaMap (fromReadOnlyRejectingChangeLens . joinChangeLens) $
    eaPair (langFiniteSetModelMeetValue seta) (langFiniteSetModelMeetValue setb)

langFiniteSetModelAdd :: LangFiniteSetModel '( p, q) -> p -> Action ()
langFiniteSetModelAdd (MkLangFiniteSetModel _ tr set) p =
    actionModelPush set $ pure $ KeyEditInsertReplace $ shimToFunction (rangeContra tr) p

langFiniteSetModelRemove :: LangFiniteSetModel '( p, q) -> p -> Action ()
langFiniteSetModelRemove (MkLangFiniteSetModel _ tr set) p =
    actionModelPush set $ pure $ KeyEditDelete $ shimToFunction (rangeContra tr) p

langFiniteSetModelRemoveAll :: LangFiniteSetModel '( BottomType, TopType) -> Action ()
langFiniteSetModelRemoveAll (MkLangFiniteSetModel _ _ set) = actionModelPush set $ pure KeyEditClear

langFiniteSetModelFunctionValue1 ::
       forall p q r. LangFiniteSetModel '( p, q) -> (forall t. (t -> q) -> WROWModel (ListSet t) -> r) -> r
langFiniteSetModelFunctionValue1 (MkLangFiniteSetModel eqv tr set) call =
    call (shimToFunction $ rangeCo tr) $ giveConstraint eqv $ eaToReadOnlyWhole set

langFiniteSetModelFunctionValue ::
       forall p q. Eq q
    => LangFiniteSetModel '( p, q)
    -> WROWModel (ListSet q)
langFiniteSetModelFunctionValue (MkLangFiniteSetModel eqv tr set) =
    eaMapReadOnlyWhole (listSetMap $ shimToFunction $ rangeCo tr) $ giveConstraint eqv $ eaToReadOnlyWhole set

langFiniteSetModelMember :: forall a. LangFiniteSetModel '( a, TopType) -> a -> LangWholeModel '( Bool, Bool)
langFiniteSetModelMember (MkLangFiniteSetModel eqv tr set) val = let
    tval = shimToFunction (rangeContra tr) val
    in wModelToWholeModel $ eaMap (wholeChangeLens knowMaybeLens . finiteSetChangeLens eqv tval) set

langFiniteSetModelSingle ::
       forall a. LangFiniteSetModel '( BottomType, MeetType Entity a) -> LangWholeModel '( TopType, a)
langFiniteSetModelSingle set =
    wROWModelToWholeModel $
    eaMapReadOnlyWhole (fmap meet2 . maybeToKnow . getSingle) $ langFiniteSetModelFunctionValue set

langFiniteSetModelCount :: LangFiniteSetModel '( BottomType, a) -> LangWholeModel '( TopType, Int)
langFiniteSetModelCount (MkLangFiniteSetModel eqv _ set) =
    wROWModelToWholeModel $ eaMapReadOnlyWhole (Known . olength) $ giveConstraint eqv $ eaToReadOnlyWhole set

langFiniteSetModelCartesianSum ::
       forall ap aq bp bq.
       LangFiniteSetModel '( ap, aq)
    -> LangFiniteSetModel '( bp, bq)
    -> LangFiniteSetModel '( Either ap bp, Either aq bq)
langFiniteSetModelCartesianSum (MkLangFiniteSetModel eqva tra vala) (MkLangFiniteSetModel eqvb trb valb) =
    MkLangFiniteSetModel (eqva <+++> eqvb) (eitherRange tra trb) $
    eaMap finiteSetCartesianSumChangeLens $ eaPair vala valb

langFiniteSetModelCartesianProduct ::
       forall ap aq bp bq.
       LangFiniteSetModel '( ap, aq)
    -> LangFiniteSetModel '( bp, bq)
    -> LangFiniteSetModel '( (ap, bp), (aq, bq))
langFiniteSetModelCartesianProduct (MkLangFiniteSetModel eqva tra vala) (MkLangFiniteSetModel eqvb trb valb) =
    MkLangFiniteSetModel (eqva <***> eqvb) (pairRange tra trb) $
    eaMap (fromReadOnlyRejectingChangeLens . finiteSetCartesianProductUpdateFunction) $ eaPair vala valb

langFiniteSetModelToSetModel :: forall p q. LangFiniteSetModel '( p, q) -> LangSetModel p
langFiniteSetModelToSetModel (MkLangFiniteSetModel eqv tr sval) =
    contramap (shimToFunction $ rangeContra tr) $ MkLangSetModel eqv $ eaMap finiteSetFunctionChangeLens sval

langFiniteSetModelSetIntersect ::
       forall p q. LangFiniteSetModel '( p, q) -> LangSetModel q -> LangFiniteSetModel '( p, q)
langFiniteSetModelSetIntersect (MkLangFiniteSetModel eqv tr fsetval) fsetref = let
    MkLangSetModel _ setval = contramap (shimToFunction $ rangeCo tr) fsetref
    in MkLangFiniteSetModel eqv tr $
       eaMap (fromReadOnlyRejectingChangeLens . filterFiniteSetUpdateFunction) $ eaPair fsetval setval

langFiniteSetModelSetDifference ::
       forall p q. LangFiniteSetModel '( p, q) -> LangSetModel q -> LangFiniteSetModel '( p, q)
langFiniteSetModelSetDifference a b = langFiniteSetModelSetIntersect a $ langSetModelComplement b

wholeListFiniteSetChangeLens :: Eq a => ChangeLens (WholeUpdate [a]) (FiniteSetUpdate a)
wholeListFiniteSetChangeLens = convertChangeLens . bijectionWholeChangeLens (MkIsomorphism setFromList toList)

langListModelToFiniteSetModel ::
       forall a. LangWholeModel '( [a], [MeetType Entity a]) -> LangFiniteSetModel '( MeetType Entity a, a)
langListModelToFiniteSetModel model =
    meetValueLangFiniteSetModel $
    eaMap
        (wholeListFiniteSetChangeLens .
         unknownValueChangeLens [] . biSingleChangeLens . mapBiWholeChangeLens (fmap $ fmap meet2) id) $
    langWholeModelToBiWholeModel model
