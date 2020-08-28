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
        MkOptionUISpec itemsModel whichModel <- isUISpec uispec
        return $ optionView itemsModel whichModel

listStoreView ::
       forall update. (ApplicableUpdate update, FullSubjectReader (UpdateReader update))
    => WIOFunction IO
    -> Model (ReadOnlyUpdate (OrderedListUpdate [UpdateSubject update] update))
    -> EditSource
    -> CreateView (SeqStore (UpdateSubject update))
listStoreView (MkWMFunction blockSignal) itemsModel esrc = let
    initV ::
           Model (ReadOnlyUpdate (OrderedListUpdate [UpdateSubject update] update))
        -> CreateView (SeqStore (UpdateSubject update))
    initV rm = do
        subjectList <- viewRunResource rm $ \am -> readableToSubject $ aModelRead am
        seqStoreNew subjectList
    recv ::
           SeqStore (UpdateSubject update)
        -> NonEmpty (ReadOnlyUpdate (OrderedListUpdate [UpdateSubject update] update))
        -> View ()
    recv store updates =
        liftIO $
        for_ updates $ \(MkReadOnlyUpdate lupdate) ->
            case lupdate of
                OrderedListUpdateItem oldi newi Nothing
                    | oldi == newi -> return ()
                OrderedListUpdateItem (fromIntegral -> oldi) (fromIntegral -> newi) mupdate -> do
                    oldval <- seqStoreGetValue store oldi
                    newval <-
                        case mupdate of
                            Just update -> readableToSubject $ applyUpdate update $ subjectToReadable oldval
                            Nothing -> return oldval
                    blockSignal $
                        case compare newi oldi of
                            EQ -> seqStoreSetValue store newi newval
                            LT -> do
                                seqStoreRemove store oldi
                                seqStoreInsert store newi newval
                            GT -> do
                                seqStoreInsert store newi newval
                                seqStoreRemove store oldi
                OrderedListUpdateDelete (fromIntegral -> i) -> blockSignal $ seqStoreRemove store i
                OrderedListUpdateInsert (fromIntegral -> i) item -> blockSignal $ seqStoreInsert store i item
                OrderedListUpdateClear -> blockSignal $ seqStoreClear store
    in cvBindModel itemsModel (Just esrc) initV mempty recv

optionUICellAttributes :: OptionUICell -> [AttrOp CellRendererText 'AttrSet]
optionUICellAttributes MkOptionUICell {..} = textCellAttributes optionCellText optionCellStyle

optionFromStore ::
       forall t. Eq t
    => Model (WholeUpdate t)
    -> EditSource
    -> SeqStore (t, OptionUICell)
    -> CreateView (WIOFunction IO, Widget)
optionFromStore whichModel esrc store = do
    widget <- comboBoxNewWithModel store
    renderer <- new CellRendererText []
    #packStart widget renderer False
    cellLayoutSetAttributes widget renderer store $ \(_, cell) -> optionUICellAttributes cell
    changedSignal <-
        cvOn widget #changed $
        viewRunResource whichModel $ \asub -> do
            mi <- #getActiveIter widget
            case mi of
                (True, iter) -> do
                    i <- seqStoreIterToIndex iter
                    (t, _) <- seqStoreGetValue store i
                    _ <- pushEdit esrc $ aModelEdit asub $ pure $ MkWholeReaderEdit t
                    return ()
                (False, _) -> return ()
    let
        blockSignal :: forall a. IO a -> IO a
        blockSignal = withSignalBlocked widget changedSignal
        findN ::
               forall l. SemiSequence l
            => [Element l -> Bool]
            -> l
            -> Maybe (Element l)
        findN [] _ = empty
        findN (p:pp) fa = find p fa <|> findN pp fa
        update :: t -> View ()
        update t =
            liftIO $ do
                items <- seqStoreToList store
                let
                    matchVal :: (Int, (t, OptionUICell)) -> Bool
                    matchVal (_, (t', _)) = t == t'
                    isDefault :: (Int, (t, OptionUICell)) -> Bool
                    isDefault (_, (_, cell)) = optionCellDefault cell
                case findN [matchVal, isDefault] $ zip [(0 :: Int) ..] items of
                    Just (i, _) -> do
                        tp <- treePathNewFromIndices [fromIntegral i]
                        mti <- treeModelGetIter store tp
                        case mti of
                            Just ti -> blockSignal $ #setActiveIter widget $ Just ti
                            Nothing -> return ()
                    Nothing -> return ()
    cvBindWholeModel whichModel (Just esrc) update
    w <- toWidget widget
    return (MkWMFunction blockSignal, w)

optionView ::
       forall update t.
       ( Eq t
       , FullSubjectReader (UpdateReader update)
       , ApplicableUpdate update
       , UpdateSubject update ~ (t, OptionUICell)
       )
    => Model (ReadOnlyUpdate (OrderedListUpdate [UpdateSubject update] update))
    -> Model (WholeUpdate t)
    -> GCreateView
optionView itemsModel whichModel = do
    esrc <- newEditSource
    rec
        store <- listStoreView blockSignal itemsModel esrc
        (blockSignal, w) <- optionFromStore whichModel esrc store
    return w
