module Pinafore.Language.Value.ListModel where

import Changes.Core
import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.Model
import Pinafore.Language.Value.WholeModel
import Shapes

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

newMemListModel :: forall a. PinaforeAction (LangListModel '( a, a))
newMemListModel = do
    r <- liftIO $ makeMemoryReference mempty $ \_ -> True
    model :: Model (ListUpdate (WholeUpdate a)) <- actionLiftLifecycle $ makeReflectingModel $ convertReference r
    uh <- pinaforeUndoHandler
    return $ FullLangListModel $ eaMap singleBiChangeLens $ MkWModel $ undoHandlerModel uh model

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

langListModelCountModel :: forall p q. LangListModel '( p, q) -> PinaforeImmutableWholeModel Int64
langListModelCountModel (OrderedLangListModel model) =
    functionImmutableModel $ eaMap (funcChangeLens coerce . orderedListLengthLens) model
langListModelCountModel (FullLangListModel model) =
    functionImmutableModel $
    eaMap (funcChangeLens coerce . liftReadOnlyChangeLens listLengthLens . biReadOnlyChangeLens) model

langListModelRead :: forall p q t. LangListModel '( p, q) -> ReadM (ListReader (WholeReader q)) t -> PinaforeAction t
langListModelRead (OrderedLangListModel model) rm = pinaforeModelGet model rm
langListModelRead (FullLangListModel model) rm = pinaforeModelGet model rm

langListModelGetCount :: forall p q. LangListModel '( p, q) -> PinaforeAction Int64
langListModelGetCount model = fmap unSequencePoint $ langListModelRead model $ readM ListReadLength

langListModelGetItem :: forall p q. Int64 -> LangListModel '( p, q) -> PinaforeAction q
langListModelGetItem i model = do
    mq <- langListModelRead model $ readM $ ListReadItem (MkSequencePoint i) ReadWhole
    pinaforeActionKnow $ maybeToKnow mq

langListModelInsert :: forall p q. Int64 -> p -> LangListModel '( p, q) -> PinaforeAction ()
langListModelInsert _ _ (OrderedLangListModel _) = empty
langListModelInsert i val (FullLangListModel model) =
    pinaforeModelPush model $ pure $ MkBiEdit $ ListEditInsert (MkSequencePoint i) val

langListModelSet :: forall p q. Int64 -> p -> LangListModel '( p, q) -> PinaforeAction ()
langListModelSet _ _ (OrderedLangListModel _) = empty
langListModelSet i val (FullLangListModel model) =
    pinaforeModelPush model $ pure $ MkBiEdit $ ListEditItem (MkSequencePoint i) $ MkWholeReaderEdit val

langListModelDelete :: forall p q. Int64 -> LangListModel '( p, q) -> PinaforeAction ()
langListModelDelete i (OrderedLangListModel model) =
    pinaforeModelPush model $ pure $ OrderedListEditDelete (MkSequencePoint i)
langListModelDelete i (FullLangListModel model) =
    pinaforeModelPush model $ pure $ MkBiEdit $ ListEditDelete (MkSequencePoint i)

langListModelClear :: forall p q. LangListModel '( p, q) -> PinaforeAction ()
langListModelClear (OrderedLangListModel model) = pinaforeModelPush model $ pure OrderedListEditClear
langListModelClear (FullLangListModel model) = pinaforeModelPush model $ pure $ MkBiEdit ListEditClear

langWholeModelToListModel :: forall a. LangWholeModel '( Vector a, Vector a) -> LangListModel '( a, a)
langWholeModelToListModel (MutableLangWholeModel model) =
    FullLangListModel $ eaMap (convertBiChangeLens id . biFromKnowWhole) model
langWholeModelToListModel (ImmutableLangWholeModel model) =
    OrderedLangListModel $
    eaMap (fromReadOnlyRejectingChangeLens . convertReadOnlyChangeLens) $ pinaforeImmutableModelValue mempty model

langListModelToWholeModel :: forall a. LangListModel '( a, a) -> LangWholeModel '( Vector a, Vector a)
langListModelToWholeModel (FullLangListModel model) =
    MutableLangWholeModel $ eaMap (biToKnowWhole . convertBiChangeLens id) model
langListModelToWholeModel (OrderedLangListModel model) =
    ImmutableLangWholeModel $ functionImmutableModel $ eaMap convertReadOnlyChangeLens model

langListModelItem :: forall p q. Bool -> Int64 -> LangListModel '( p, q) -> PinaforeAction (LangWholeModel '( p, q))
langListModelItem present i (FullLangListModel lmodel) = do
    let
        linearListItemCL :: forall t. LinearFloatingChangeLens _ (ListUpdate (WholeUpdate t)) (WholeUpdate (Know t))
        linearListItemCL =
            composeExpFloatingChangeLens (changeLensToExpFloating $ bijectionWholeChangeLens $ invert knowMaybe) $
            listItemLinearLens present $ MkSequencePoint i
    wmodel <-
        pinaforeFloatMap
            (expToFloatingChangeLens $ biLinearFloatingChangeLens (linearListItemCL @p) (linearListItemCL @q))
            lmodel
    return $ MutableLangWholeModel wmodel
langListModelItem _present i (OrderedLangListModel lmodel) = do
    let ip = MkSequencePoint i
    wmodel <- pinaforeFloatMap (changeLensToFloating (funcChangeLens maybeToKnow) . orderedListItemLens ip) lmodel
    return $ ImmutableLangWholeModel $ MkPinaforeImmutableWholeModel wmodel
