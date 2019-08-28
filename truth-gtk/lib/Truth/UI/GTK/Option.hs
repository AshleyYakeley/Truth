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
        MkOptionUISpec itemsFunction whichLens <- isUISpec uispec
        return $ optionView itemsFunction whichLens

listStoreView ::
       (FullSubjectReader (EditReader edit), ApplicableEdit edit)
    => UnliftIO IO
    -> EditSource
    -> CreateView sel (ListEdit [EditSubject edit] edit) (SeqStore (EditSubject edit))
listStoreView (MkTransform blockSignal) esrc = do
    subjectList <- cvLiftView $ viewObjectRead $ \_ -> mutableReadToSubject
    store <- seqStoreNew subjectList
    cvReceiveUpdate (Just esrc) $ \_ _mr ->
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
       forall sel t. Eq t
    => EditSource
    -> SeqStore (t, Text)
    -> CreateView sel (WholeEdit t) (UnliftIO IO, Widget)
optionFromStore esrc store = do
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
                    _ <- push esrc [MkWholeEdit t]
                    return ()
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
    cvReceiveUpdate (Just esrc) $ \_ _mr (MkWholeEdit t) -> update t
    w <- toWidget widget
    return (MkTransform blockSignal, w)

optionView ::
       forall t tedit sel. (Eq t)
    => UpdateFunction tedit (ListEdit [(t, Text)] (WholeEdit (t, Text)))
    -> EditLens tedit (WholeEdit t)
    -> GCreateView sel tedit
optionView itemsFunction whichLens = do
    esrc <- newEditSource
    rec
        store <- cvMapEdit (readOnlyEditLens itemsFunction) $ listStoreView blockSignal esrc
        (blockSignal, w) <- cvMapEdit whichLens $ optionFromStore esrc store
    return w
