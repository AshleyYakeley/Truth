module Pinafore.Language.Value.ListModel where

import Import
import Pinafore.Language.Value.Model
import Pinafore.Language.Value.WholeModel

type LangListModel :: (Type, Type) -> Type
data LangListModel pq where
    OrderedLangListModel :: WModel (OrderedListUpdate (ROWUpdate q)) -> LangListModel '( p, q)
    FullLangListModel
        :: WModel (BiUpdate (ListUpdate (WholeUpdate p)) (ListUpdate (WholeUpdate q))) -> LangListModel '( p, q)

instance CatFunctor (CatRange (->)) (->) LangListModel where
    cfmap (MkCatRange _ qq) (OrderedLangListModel model) =
        OrderedLangListModel $ eaMap (liftOrderedListChangeLens $ funcChangeLens qq) model
    cfmap (MkCatRange pp qq) (FullLangListModel model) =
        FullLangListModel $ eaMap (mappingBiChangeLens (listWholeChangeMap pp) (listWholeChangeMap qq)) model

instance MaybeRepresentational LangListModel where
    maybeRepresentational = Nothing

instance HasCCRVariance 'RangeCCRVariance LangListModel

instance IsInvertibleModel (LangListModel '( t, t)) where
    invertibleModelLens f (OrderedLangListModel model) = fmap OrderedLangListModel $ wUninvertibleModelLens f model
    invertibleModelLens f (FullLangListModel model) = fmap FullLangListModel $ wInvertibleModelLens f model

newMemListModel :: forall a. Action (LangListModel '( a, a))
newMemListModel = do
    r <- liftIO $ makeMemoryReference mempty $ \_ -> True
    model :: Model (ListUpdate (WholeUpdate a)) <- actionLiftLifecycle $ makeReflectingModel $ convertReference r
    return $ FullLangListModel $ eaMap singleBiChangeLens $ MkWModel model

langListModelToModel :: forall p q. LangListModel '( p, q) -> LangModel
langListModelToModel (OrderedLangListModel model) = MkLangModel model
langListModelToModel (FullLangListModel model) = MkLangModel model

langListModelToOrdered :: forall p q. LangListModel '( p, q) -> WModel (OrderedListUpdate (ROWUpdate q))
langListModelToOrdered (OrderedLangListModel model) = model
langListModelToOrdered (FullLangListModel model) =
    eaMap
        (liftOrderedListChangeLens toReadOnlyChangeLens .
         listOrderedListChangeLens . fromReadOnlyRejectingChangeLens . biReadOnlyChangeLens)
        model

langImmutListModel :: forall p q r. LangListModel '( p, q) -> LangListModel '( r, q)
langImmutListModel model = OrderedLangListModel $ langListModelToOrdered model

langListModelCountModel :: forall p q. LangListModel '( p, q) -> ImmutableWholeModel Natural
langListModelCountModel (OrderedLangListModel model) =
    functionImmutableModel $ eaMap (funcChangeLens (toNaturalForce . unSequencePoint) . orderedListLengthLens) model
langListModelCountModel (FullLangListModel model) =
    functionImmutableModel $
    eaMap
        (funcChangeLens (toNaturalForce . unSequencePoint) .
         liftReadOnlyChangeLens listLengthLens . biReadOnlyChangeLens)
        model

langListModelRead :: forall p q t. LangListModel '( p, q) -> ReadM (ListReader (WholeReader q)) t -> Action t
langListModelRead (OrderedLangListModel model) rm = actionModelGet model rm
langListModelRead (FullLangListModel model) rm = actionModelGet model rm

langListModelGetCount :: forall p q. LangListModel '( p, q) -> Action Natural
langListModelGetCount model = fmap (toNaturalForce . unSequencePoint) $ langListModelRead model $ readM ListReadLength

langListModelGetItem :: forall p q. SequencePoint -> LangListModel '( p, q) -> Action q
langListModelGetItem i model = do
    mq <- langListModelRead model $ readM $ ListReadItem i ReadWhole
    actionKnow $ maybeToKnow mq

langListModelInsert :: forall p q. SequencePoint -> p -> LangListModel '( p, q) -> Action ()
langListModelInsert _ _ (OrderedLangListModel _) = empty
langListModelInsert i val (FullLangListModel model) = actionModelPush model $ pure $ MkBiEdit $ ListEditInsert i val

langListModelSet :: forall p q. SequencePoint -> p -> LangListModel '( p, q) -> Action ()
langListModelSet _ _ (OrderedLangListModel _) = empty
langListModelSet i val (FullLangListModel model) =
    actionModelPush model $ pure $ MkBiEdit $ ListEditItem i $ MkWholeReaderEdit val

langListModelDelete :: forall p q. SequencePoint -> LangListModel '( p, q) -> Action ()
langListModelDelete i (OrderedLangListModel model) = actionModelPush model $ pure $ OrderedListEditDelete i
langListModelDelete i (FullLangListModel model) = actionModelPush model $ pure $ MkBiEdit $ ListEditDelete i

langListModelClear :: forall p q. LangListModel '( p, q) -> Action ()
langListModelClear (OrderedLangListModel model) = actionModelPush model $ pure OrderedListEditClear
langListModelClear (FullLangListModel model) = actionModelPush model $ pure $ MkBiEdit ListEditClear

langWholeModelToListModel :: forall a. LangWholeModel '( Vector a, Vector a) -> LangListModel '( a, a)
langWholeModelToListModel (MutableLangWholeModel model) =
    FullLangListModel $ eaMap (convertBiChangeLens id . biFromKnowWhole) model
langWholeModelToListModel (ImmutableLangWholeModel model) =
    OrderedLangListModel $
    eaMap (fromReadOnlyRejectingChangeLens . convertReadOnlyChangeLens) $ immutableWholeModelValue mempty model

langListModelToWholeModel :: forall a. LangListModel '( a, a) -> LangWholeModel '( Vector a, Vector a)
langListModelToWholeModel (FullLangListModel model) =
    MutableLangWholeModel $ eaMap (biToKnowWhole . convertBiChangeLens id) model
langListModelToWholeModel (OrderedLangListModel model) =
    ImmutableLangWholeModel $ functionImmutableModel $ eaMap convertReadOnlyChangeLens model

langListModelItem :: forall p q. Bool -> SequencePoint -> LangListModel '( p, q) -> Action (LangWholeModel '( p, q))
langListModelItem present i (FullLangListModel lmodel) = do
    let
        linearListItemCL :: forall t. LinearFloatingChangeLens _ (ListUpdate (WholeUpdate t)) (WholeUpdate (Know t))
        linearListItemCL =
            composeExpFloatingChangeLens (changeLensToExpFloating $ bijectionWholeChangeLens $ invert knowMaybe) $
            listItemLinearLens present i
    wmodel <-
        actionFloatMap
            (expToFloatingChangeLens $ biLinearFloatingChangeLens (linearListItemCL @p) (linearListItemCL @q))
            lmodel
    return $ MutableLangWholeModel wmodel
langListModelItem _present i (OrderedLangListModel lmodel) = do
    wmodel <- actionFloatMap (changeLensToFloating (funcChangeLens maybeToKnow) . orderedListItemLens i) lmodel
    return $ ImmutableLangWholeModel $ MkImmutableWholeModel wmodel
