module Truth.UI.GTK.Option
    ( optionGetView
    ) where

import Data.GI.Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

optionGetView :: GetGView
optionGetView =
    MkGetView $ \_ uispec -> do
        MkUIOption itemsFunction whichLens <- isUISpec uispec
        return $ optionView itemsFunction whichLens

listStoreView ::
       (FullSubjectReader (EditReader edit), ApplicableEdit edit)
    => CreateView (ListEdit [EditSubject edit] edit) (SeqStore (EditSubject edit))
listStoreView = do
    subjectList <- liftOuter $ viewObjectRead mutableReadToSubject
    store <- seqStoreNew subjectList
    createViewReceiveUpdate $ \_mr ->
        \case
            ListEditItem (MkSequencePoint (fromIntegral -> i)) edit -> do
                oldval <- seqStoreGetValue store i
                newval <- mutableReadToSubject $ applyEdit edit $ subjectToMutableRead oldval
                seqStoreSetValue store i newval
            ListDeleteItem (MkSequencePoint (fromIntegral -> i)) -> seqStoreRemove store i
            ListInsertItem (MkSequencePoint (fromIntegral -> i)) item -> seqStoreInsert store i item
            ListClear -> seqStoreClear store
    return store

optionFromStore ::
       forall t. Eq t
    => SeqStore (t, Text)
    -> GCreateView (WholeEdit t)
optionFromStore store = do
    widget <-
        liftIO $ do
            widget <- comboBoxNewWithModel store
            renderer <- new CellRendererText []
            #packStart widget renderer False
            cellLayoutSetAttributes widget renderer store $ \(_, row) -> [#text := row]
            return widget
    changedSignal <-
        liftOuter $
        viewOn widget #changed $
        viewObjectPushEdit $ \push -> do
            mi <- #getActiveIter widget
            case mi of
                (True, iter) -> do
                    i <- seqStoreIterToIndex iter
                    (t, _) <- seqStoreGetValue store i
                    push [MkWholeEdit t]
                (False, _) -> return ()
    let
        update :: MonadIO m => t -> m ()
        update t =
            liftIO $ do
                items <- seqStoreToList store
                case find (\(_, (t', _)) -> t == t') $ zip [(0 :: Int) ..] items of
                    Just (i, _) -> do
                        tp <- treePathNewFromIndices [fromIntegral i]
                        mti <- treeModelGetIter store tp
                        case mti of
                            Just ti -> withSignalBlocked widget changedSignal $ #setActiveIter widget $ Just ti
                            Nothing -> return ()
                    Nothing -> return ()
    liftOuter $
        viewObjectRead $ \mr -> do
            t <- mr ReadWhole
            update t
    createViewReceiveUpdate $ \_mr (MkWholeEdit t) -> update t
    toWidget widget

optionView ::
       forall t tedit. (Eq t)
    => EditFunction tedit (ListEdit [(t, Text)] (WholeEdit (t, Text)))
    -> EditLens tedit (WholeEdit t)
    -> GCreateView tedit
optionView itemsFunction whichLens = do
    store <- mapCreateViewEdit (readOnlyEditLens itemsFunction) listStoreView
    mapCreateViewEdit whichLens $ optionFromStore store
