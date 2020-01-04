module Truth.UI.GTK.Option
    ( optionGetView
    ) where

import Data.GI.Base.Attributes
import Data.GI.Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.TextStyle
import Truth.UI.GTK.Useful

optionGetView :: GetGView
optionGetView =
    MkGetView $ \_ uispec -> do
        MkOptionUISpec itemsSub whichSub <- isUISpec uispec
        return $ optionView itemsSub whichSub

listStoreView ::
       forall sel update.
       (IsEditUpdate update, FullSubjectReader (UpdateReader update), ApplicableEdit (UpdateEdit update))
    => WIOFunction IO
    -> ReadOnlyOpenSubscriber (ListUpdate [UpdateSubject update] update)
    -> EditSource
    -> CreateView sel (SeqStore (UpdateSubject update))
listStoreView (MkWMFunction blockSignal) oobj esrc = let
    initV ::
           ReadOnlyOpenSubscriber (ListUpdate [UpdateSubject update] update)
        -> CreateView sel (SeqStore (UpdateSubject update))
    initV rm = do
        subjectList <- liftIO $ withOpenResource rm $ \am -> mutableReadToSubject $ subRead am
        seqStoreNew subjectList
    recv ::
           SeqStore (UpdateSubject update)
        -> NonEmpty (ReadOnlyUpdate (ListUpdate [UpdateSubject update] update))
        -> IO ()
    recv store updates =
        for_ updates $ \(MkReadOnlyUpdate lupdate) ->
            case lupdate of
                ListUpdateItem (MkSequencePoint (fromIntegral -> i)) update -> do
                    oldval <- seqStoreGetValue store i
                    newval <- mutableReadToSubject $ applyEdit (updateEdit update) $ subjectToMutableRead oldval
                    blockSignal $ seqStoreSetValue store i newval
                ListUpdateDelete (MkSequencePoint (fromIntegral -> i)) -> blockSignal $ seqStoreRemove store i
                ListUpdateInsert (MkSequencePoint (fromIntegral -> i)) item -> blockSignal $ seqStoreInsert store i item
                ListUpdateClear -> blockSignal $ seqStoreClear store
    in cvBindSubscriber oobj (Just esrc) initV recv

optionUICellAttributes :: OptionUICell -> [AttrOp CellRendererText 'AttrSet]
optionUICellAttributes MkOptionUICell {..} = textCellAttributes optionCellText optionCellStyle

optionFromStore ::
       forall sel t. Eq t
    => OpenSubscriber (WholeUpdate t)
    -> EditSource
    -> SeqStore (t, OptionUICell)
    -> CreateView sel (WIOFunction IO, Widget)
optionFromStore oobj@(MkOpenResource _ run asub) esrc store = do
    widget <- comboBoxNewWithModel store
    renderer <- new CellRendererText []
    #packStart widget renderer False
    cellLayoutSetAttributes widget renderer store $ \(_, cell) -> optionUICellAttributes cell
    changedSignal <-
        cvLiftView $
        viewOn widget #changed $
        liftIO $
        run $ do
            mi <- #getActiveIter widget
            case mi of
                (True, iter) -> do
                    i <- seqStoreIterToIndex iter
                    (t, _) <- seqStoreGetValue store i
                    _ <- pushEdit esrc $ subEdit asub $ pure $ MkWholeReaderEdit t
                    return ()
                (False, _) -> return ()
    let
        blockSignal :: forall a. IO a -> IO a
        blockSignal = withSignalBlocked widget changedSignal
        update :: t -> IO ()
        update t = do
            items <- seqStoreToList store
            case find (\(_, (t', _)) -> t == t') $ zip [(0 :: Int) ..] items of
                Just (i, _) -> do
                    tp <- treePathNewFromIndices [fromIntegral i]
                    mti <- treeModelGetIter store tp
                    case mti of
                        Just ti -> blockSignal $ #setActiveIter widget $ Just ti
                        Nothing -> return ()
                Nothing -> return ()
    cvBindWholeSubscriber oobj (Just esrc) update
    w <- toWidget widget
    return (MkWMFunction blockSignal, w)

optionView ::
       forall t sel. (Eq t)
    => ReadOnlyOpenSubscriber (ListUpdate [(t, OptionUICell)] (WholeUpdate (t, OptionUICell)))
    -> OpenSubscriber (WholeUpdate t)
    -> GCreateView sel
optionView itemsSub whichSub = do
    esrc <- newEditSource
    rec
        store <- listStoreView blockSignal itemsSub esrc
        (blockSignal, w) <- optionFromStore whichSub esrc store
    return w
