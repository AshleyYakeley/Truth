module Pinafore.Language.Value.FiniteSetModel where

import Changes.Core
import Data.Shim
import Pinafore.Base
import Pinafore.Language.Shim
import Pinafore.Language.Value.Model
import Pinafore.Language.Value.SetModel
import Pinafore.Language.Value.WholeModel
import Shapes

data LangFiniteSetModel pq where
    MkLangFiniteSetModel :: Eq t => Range (QPolyShim Type) t pq -> WModel (FiniteSetUpdate t) -> LangFiniteSetModel pq

unLangFiniteSetModel :: LangFiniteSetModel '( p, p) -> WModel (FiniteSetUpdate p)
unLangFiniteSetModel (MkLangFiniteSetModel tr lv) =
    eaMap (bijectionFiniteSetChangeLens $ isoMapCat shimToFunction $ rangeBijection tr) lv

instance CatFunctor (CatRange (->)) (->) LangFiniteSetModel where
    cfmap f (MkLangFiniteSetModel r v) = MkLangFiniteSetModel (cfmap f r) v

instance MaybeRepresentational LangFiniteSetModel where
    maybeRepresentational = Nothing

instance HasCCRVariance 'RangeCCRVariance LangFiniteSetModel

instance IsInvertibleModel (LangFiniteSetModel pq) where
    invertibleModelLens f (MkLangFiniteSetModel tr model) =
        fmap (MkLangFiniteSetModel tr) $ wInvertibleModelLens f model

newMemFiniteSetModel :: forall a. Action (LangFiniteSetModel '( MeetType Entity a, a))
newMemFiniteSetModel = do
    r <- liftIO $ makeMemoryReference mempty $ \_ -> True
    model <- actionLiftLifecycle $ makeReflectingModel $ convertReference r
    return $ meetValueLangFiniteSetModel $ MkWModel model

langFiniteSetModelValue :: LangFiniteSetModel '( q, q) -> WModel (FiniteSetUpdate q)
langFiniteSetModelValue (MkLangFiniteSetModel tr lv) =
    eaMap (bijectionFiniteSetChangeLens (isoMapCat shimToFunction $ rangeBijection tr)) lv

valueLangFiniteSetModel :: Eq q => WModel (FiniteSetUpdate q) -> LangFiniteSetModel '( q, q)
valueLangFiniteSetModel lv = MkLangFiniteSetModel identityRange lv

langFiniteSetFilter :: forall ap aq. (aq -> Bool) -> LangFiniteSetModel '( ap, aq) -> LangFiniteSetModel '( ap, aq)
langFiniteSetFilter f (MkLangFiniteSetModel tr lv) =
    MkLangFiniteSetModel tr $ eaMap (filterFiniteSetChangeLens $ f . shimToFunction (rangeCo tr)) lv

langFiniteSetMaybeMap ::
       forall ap aq b. (aq -> Maybe b) -> LangFiniteSetModel '( ap, aq) -> LangFiniteSetModel '( MeetType ap b, b)
langFiniteSetMaybeMap f (MkLangFiniteSetModel (tr :: _ t _) lv) = let
    tr' :: Range (QPolyShim Type) (MeetType t b) '( MeetType ap b, b)
    tr' = MkRange (iMeetPair (rangeContra tr) id) meet2
    amb :: t -> Maybe (MeetType t b)
    amb t = do
        b <- f $ shimToFunction (rangeCo tr) t
        return $ BothMeetType t b
    in MkLangFiniteSetModel tr' $ eaMap (mapMaybeFiniteSetChangeLens amb meet1) lv

langFiniteSetCollect ::
       forall ap aq b.
       ImmutableWholeModel (aq -> Maybe b)
    -> LangFiniteSetModel '( ap, aq)
    -> LangFiniteSetModel '( MeetType ap b, b)
langFiniteSetCollect (MkImmutableWholeModel mf) (MkLangFiniteSetModel (tr :: _ t _) lv) = let
    tr' :: Range (QPolyShim Type) (MeetType t b) '( MeetType ap b, b)
    tr' = MkRange (iMeetPair (rangeContra tr) id) meet2
    amb :: Know (aq -> Maybe b) -> (t -> Maybe (MeetType t b))
    amb Unknown _ = Nothing
    amb (Known f) t = do
        b <- f $ shimToFunction (rangeCo tr) t
        return $ BothMeetType t b
    mf' = eaMapReadOnlyWhole amb mf
    in MkLangFiniteSetModel tr' $ eaMap (collectFiniteSetChangeLens meet1) $ eaPair mf' lv

langFiniteSetModelMeetValue ::
       LangFiniteSetModel '( t, MeetType Entity t) -> WModel (FiniteSetUpdate (MeetType Entity t))
langFiniteSetModelMeetValue (MkLangFiniteSetModel tr lv) =
    langFiniteSetModelValue $ MkLangFiniteSetModel (contraMapRange meet2 tr) lv

meetValueLangFiniteSetModel ::
       WModel (FiniteSetUpdate (MeetType Entity t)) -> LangFiniteSetModel '( MeetType Entity t, t)
meetValueLangFiniteSetModel lv = MkLangFiniteSetModel (coMapRange meet2 identityRange) lv

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
langFiniteSetModelAdd (MkLangFiniteSetModel tr set) p =
    actionModelPush set $ pure $ KeyEditInsertReplace $ shimToFunction (rangeContra tr) p

langFiniteSetModelRemove :: LangFiniteSetModel '( p, q) -> p -> Action ()
langFiniteSetModelRemove (MkLangFiniteSetModel tr set) p =
    actionModelPush set $ pure $ KeyEditDelete $ shimToFunction (rangeContra tr) p

langFiniteSetModelRemoveAll :: LangFiniteSetModel '( BottomType, TopType) -> Action ()
langFiniteSetModelRemoveAll (MkLangFiniteSetModel _ set) = actionModelPush set $ pure KeyEditClear

langFiniteSetModelFunctionValue :: LangFiniteSetModel '( t, a) -> WROWModel (FiniteSet a)
langFiniteSetModelFunctionValue (MkLangFiniteSetModel tr set) =
    eaMapReadOnlyWhole (fmap $ shimToFunction $ rangeCo tr) $ eaToReadOnlyWhole set

langFiniteSetModelMember :: forall a. LangFiniteSetModel '( a, TopType) -> a -> LangWholeModel '( Bool, Bool)
langFiniteSetModelMember (MkLangFiniteSetModel tr set) val = let
    tval = shimToFunction (rangeContra tr) val
    in wModelToWholeModel $ eaMap (wholeChangeLens knowMaybeLens . finiteSetChangeLens tval) set

langFiniteSetModelSingle ::
       forall a. LangFiniteSetModel '( BottomType, MeetType Entity a) -> LangWholeModel '( TopType, a)
langFiniteSetModelSingle set =
    wROWModelToWholeModel $
    eaMapReadOnlyWhole (fmap meet2 . maybeToKnow . getSingle) $ langFiniteSetModelFunctionValue set

langFiniteSetModelFunc ::
       forall a b. (FiniteSet a -> b) -> LangFiniteSetModel '( BottomType, a) -> LangWholeModel '( TopType, b)
langFiniteSetModelFunc f set =
    wROWModelToWholeModel $ eaMapReadOnlyWhole (Known . f) $ langFiniteSetModelFunctionValue set

langFiniteSetModelCartesianSum ::
       forall ap aq bp bq.
       LangFiniteSetModel '( ap, aq)
    -> LangFiniteSetModel '( bp, bq)
    -> LangFiniteSetModel '( Either ap bp, Either aq bq)
langFiniteSetModelCartesianSum (MkLangFiniteSetModel tra vala) (MkLangFiniteSetModel trb valb) =
    MkLangFiniteSetModel (eitherRange tra trb) $ eaMap finiteSetCartesianSumChangeLens $ eaPair vala valb

langFiniteSetModelCartesianProduct ::
       forall ap aq bp bq.
       LangFiniteSetModel '( ap, aq)
    -> LangFiniteSetModel '( bp, bq)
    -> LangFiniteSetModel '( (ap, bp), (aq, bq))
langFiniteSetModelCartesianProduct (MkLangFiniteSetModel tra vala) (MkLangFiniteSetModel trb valb) =
    MkLangFiniteSetModel (pairRange tra trb) $
    eaMap (fromReadOnlyRejectingChangeLens . finiteSetCartesianProductUpdateFunction) $ eaPair vala valb

langFiniteSetModelToSetModel :: forall p q. LangFiniteSetModel '( p, q) -> LangSetModel p
langFiniteSetModelToSetModel (MkLangFiniteSetModel tr sval) =
    contramap (shimToFunction $ rangeContra tr) $ MkLangSetModel (==) $ eaMap finiteSetFunctionChangeLens sval

langFiniteSetModelSetIntersect ::
       forall p q. LangFiniteSetModel '( p, q) -> LangSetModel q -> LangFiniteSetModel '( p, q)
langFiniteSetModelSetIntersect (MkLangFiniteSetModel tr fsetval) fsetref = let
    MkLangSetModel _ setval = contramap (shimToFunction $ rangeCo tr) fsetref
    in MkLangFiniteSetModel tr $
       eaMap (fromReadOnlyRejectingChangeLens . filterFiniteSetUpdateFunction) $ eaPair fsetval setval

langFiniteSetModelSetDifference ::
       forall p q. LangFiniteSetModel '( p, q) -> LangSetModel q -> LangFiniteSetModel '( p, q)
langFiniteSetModelSetDifference a b = langFiniteSetModelSetIntersect a $ langSetModelComplement b

wholeListFiniteSetChangeLens :: Eq a => ChangeLens (WholeUpdate [a]) (FiniteSetUpdate a)
wholeListFiniteSetChangeLens = convertChangeLens . bijectionWholeChangeLens coerceIsomorphism

langListModelToFiniteSetModel ::
       forall a. LangWholeModel '( [a], [MeetType Entity a]) -> LangFiniteSetModel '( MeetType Entity a, a)
langListModelToFiniteSetModel model =
    meetValueLangFiniteSetModel $
    eaMap
        (wholeListFiniteSetChangeLens .
         unknownValueChangeLens [] . biSingleChangeLens . mapBiWholeChangeLens (fmap $ fmap meet2) id) $
    langWholeModelToBiWholeModel model
