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
    => UnliftIO IO
    -> CreateView seledit (ListEdit [EditSubject edit] edit) (SeqStore (EditSubject edit))
listStoreView (MkTransform blockSignal) = do
    subjectList <- cvLiftView $ viewObjectRead $ \_ -> mutableReadToSubject
    store <- seqStoreNew subjectList
    cvReceiveUpdate $ \_ _mr ->
        \case
            ListEditItem (MkSequencePoint (fromIntegral -> i)) edit -> do
                oldval <- seqStoreGetValue store i
                newval <- mutableReadToSubject $ applyEdit edit $ subjectToMutableRead oldval
                liftIO $ blockSignal $ seqStoreSetValue store i newval
            ListDeleteItem (MkSequencePoint (fromIntegral -> i)) -> liftIO $ blockSignal $ seqStoreRemove store i
            ListInsertItem (MkSequencePoint (fromIntegral -> i)) item ->
                liftIO $ blockSignal $ seqStoreInsert store i item
            ListClear -> liftIO $ blockSignal $ seqStoreClear store
    return store

optionFromStore ::
       forall seledit t. Eq t
    => SeqStore (t, Text)
    -> CreateView seledit (WholeEdit t) (UnliftIO IO, Widget)
optionFromStore store = do
    widget <- comboBoxNewWithModel store
    renderer <- new CellRendererText []
    #packStart widget renderer False
    cellLayoutSetAttributes widget renderer store $ \(_, row) -> [#text := row]
    changedSignal <-
        cvLiftView $
        viewOn widget #changed $
        viewObjectPushEdit $ \_ push -> do
            mi <- #getActiveIter widget
            case mi of
                (True, iter) -> do
                    i <- seqStoreIterToIndex iter
                    (t, _) <- seqStoreGetValue store i
                    push [MkWholeEdit t]
                (False, _) -> return ()
    let
        blockSignal :: forall a. IO a -> IO a
        blockSignal = withSignalBlocked widget changedSignal
        update :: MonadIO m => t -> m ()
        update t =
            liftIO $ do
                items <- seqStoreToList store
                case find (\(_, (t', _)) -> t == t') $ zip [(0 :: Int) ..] items of
                    Just (i, _) -> do
                        tp <- treePathNewFromIndices [fromIntegral i]
                        mti <- treeModelGetIter store tp
                        case mti of
                            Just ti -> blockSignal $ #setActiveIter widget $ Just ti
                            Nothing -> return ()
                    Nothing -> return ()
    cvLiftView $
        viewObjectRead $ \_ mr -> do
            t <- mr ReadWhole
            update t
    cvReceiveUpdate $ \_ _mr (MkWholeEdit t) -> update t
    w <- toWidget widget
    return (MkTransform blockSignal, w)

optionView ::
       forall t tedit seledit. (Eq t)
    => EditFunction tedit (ListEdit [(t, Text)] (WholeEdit (t, Text)))
    -> EditLens tedit (WholeEdit t)
    -> GCreateView seledit tedit
optionView itemsFunction whichLens = do
    rec
        store <- cvMapEdit (readOnlyEditLens itemsFunction) $ listStoreView blockSignal
        (blockSignal, w) <- cvMapEdit whichLens $ optionFromStore store
    return w
