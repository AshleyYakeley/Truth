module Truth.UI.GTK.Option
    ( optionGetView
    ) where

import Graphics.UI.Gtk as Gtk
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
       (FullSubjectReader (EditReader edit), Edit edit)
    => CreateView (ListEdit [EditSubject edit] edit) (ListStore (EditSubject edit))
listStoreView = do
    initialList <- liftOuter $ viewObjectRead mutableReadToSubject
    store <- liftIO $ listStoreNew initialList
    createViewReceiveUpdate $ \_mr ->
        \case
            ListEditItem (MkSequencePoint i) edit ->
                liftIO $ do
                    oldval <- listStoreGetValue store i
                    newval <- mutableReadToSubject $ applyEdit edit $ subjectToMutableRead oldval
                    listStoreSetValue store i newval
            ListDeleteItem (MkSequencePoint i) -> liftIO $ listStoreRemove store i
            ListInsertItem (MkSequencePoint i) item -> liftIO $ listStoreInsert store i item
            ListClear -> liftIO $ listStoreClear store
    return store

optionFromStore ::
       forall t. Eq t
    => ListStore (t, String)
    -> GCreateView (WholeEdit t)
optionFromStore store = do
    widget <-
        liftIO $ do
            widget <- comboBoxNewWithModel store
            renderer <- cellRendererTextNew
            cellLayoutPackStart widget renderer False
            cellLayoutSetAttributes widget renderer store $ \(_, row) -> [cellText := row]
            return widget
    changedSignal <-
        liftOuter $
        viewOn widget changed $
        viewObjectPushEdit $ \push -> do
            mi <- liftIO $ comboBoxGetActiveIter widget
            case mi of
                Just i -> do
                    (t, _) <- liftIO $ listStoreGetValue store $ listStoreIterToIndex i
                    push [MkWholeEdit t]
                Nothing -> return ()
    let
        update :: MonadIO m => t -> m ()
        update t =
            liftIO $ do
                items <- listStoreToList store
                case find (\(_, (t', _)) -> t == t') $ zip [(0 :: Int) ..] items of
                    Just (i, _) -> do
                        mti <- treeModelGetIter store [i]
                        case mti of
                            Just ti -> withSignalBlocked changedSignal $ comboBoxSetActiveIter widget ti
                            Nothing -> return ()
                    Nothing -> return ()
    liftOuter $
        viewObjectRead $ \mr -> do
            t <- mr ReadWhole
            update t
    createViewReceiveUpdate $ \_mr (MkWholeEdit t) -> update t
    return $ toWidget widget

optionView ::
       forall t tedit. (Eq t, Edit tedit)
    => EditFunction tedit (ListEdit [(t, String)] (WholeEdit (t, String)))
    -> EditLens tedit (WholeEdit t)
    -> GCreateView tedit
optionView itemsFunction whichLens = do
    store <- mapCreateViewEdit (readOnlyEditLens itemsFunction) listStoreView
    mapCreateViewEdit whichLens $ optionFromStore store
