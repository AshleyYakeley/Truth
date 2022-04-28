module Changes.UI.GTK.Option
    ( ComboBoxCell(..)
    , plainComboBoxCell
    , createComboBox
    ) where

import Changes.Core
import Changes.GI
import Changes.UI.GTK.TextStyle
import Data.GI.Base.Attributes
import Data.GI.Gtk
import Shapes
import Changes.Debug.Reference

data ComboBoxCell = MkComboBoxCell
    { cbcText :: Text
    , cbcStyle :: TextStyle
    , cbcDefault :: Bool
    } deriving (Eq)

plainComboBoxCell :: Text -> ComboBoxCell
plainComboBoxCell cbcText = let
    cbcStyle = plainTextStyle
    cbcDefault = False
    in MkComboBoxCell {..}

listStoreView ::
       forall update. (IsEditUpdate update, ApplicableEdit (UpdateEdit update), FullSubjectReader (UpdateReader update))
    => WMFunction View View
    -> Model (ReadOnlyUpdate (OrderedListUpdate update))
    -> EditSource
    -> View (SeqStore (UpdateSubject update))
listStoreView (MkWMFunction blockSignal) itemsModel esrc = let
    initV :: View (SeqStore (UpdateSubject update))
    initV = do
        subjects <- viewRunResource itemsModel $ \am -> readableToSubject $ aModelRead am
        seqStoreNew $ toList subjects
    recv :: SeqStore (UpdateSubject update) -> NonEmpty (ReadOnlyUpdate (OrderedListUpdate update)) -> View ()
    recv store updates =
        for_ updates $ \(MkReadOnlyUpdate lupdate) -> traceBracket "GTK.Option:ListStore:receiveUpdate" $
            case lupdate of
                OrderedListUpdateItem oldi newi []
                    | oldi == newi -> return ()
                OrderedListUpdateItem (fromIntegral -> oldi) (fromIntegral -> newi) iupdates -> do
                    oldval <- seqStoreGetValue store oldi
                    newval <- readableToSubject $ applyEdits (fmap updateEdit iupdates) $ subjectToReadable oldval
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
    in viewBindModel itemsModel (Just esrc) initV mempty recv

comboBoxCellAttributes :: ComboBoxCell -> [AttrOp CellRendererText 'AttrSet]
comboBoxCellAttributes MkComboBoxCell {..} = textCellAttributes cbcText cbcStyle

cboxFromStore ::
       forall t. Eq t
    => Model (WholeUpdate t)
    -> EditSource
    -> SeqStore (t, ComboBoxCell)
    -> View (WMFunction View View, Widget)
cboxFromStore whichModel esrc store = do
    widget <- comboBoxNewWithModel store
    renderer <- cvNew CellRendererText []
    #packStart widget renderer False
    cellLayoutSetAttributes widget renderer store $ \(_, cell) -> comboBoxCellAttributes cell
    changedSignal <-
        viewOn widget #changed $
        traceBracket "GTK.Option:changed" $
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
        blockSignal :: View --> View
        blockSignal = withSignalBlocked widget changedSignal
        findN ::
               forall l. SemiSequence l
            => [Element l -> Bool]
            -> l
            -> Maybe (Element l)
        findN [] _ = empty
        findN (p:pp) fa = find p fa <|> findN pp fa
        update :: t -> View ()
        update t = traceBracket "GTK.Option:receiveUpdate" $ do
            items <- seqStoreToList store
            let
                matchVal :: (Int, (t, ComboBoxCell)) -> Bool
                matchVal (_, (t', _)) = t == t'
                isDefault :: (Int, (t, ComboBoxCell)) -> Bool
                isDefault (_, (_, cell)) = cbcDefault cell
            case findN [matchVal, isDefault] $ zip [(0 :: Int) ..] items of
                Just (i, _) -> do
                    tp <- treePathNewFromIndices [fromIntegral i]
                    mti <- treeModelGetIter store tp
                    case mti of
                        Just ti -> blockSignal $ #setActiveIter widget $ Just ti
                        Nothing -> return ()
                Nothing -> return ()
    viewBindWholeModel whichModel (Just esrc) update
    w <- toWidget widget
    return (MkWMFunction blockSignal, w)

createComboBox ::
       forall update t.
       ( Eq t
       , FullSubjectReader (UpdateReader update)
       , IsEditUpdate update
       , ApplicableEdit (UpdateEdit update)
       , UpdateSubject update ~ (t, ComboBoxCell)
       )
    => Model (ReadOnlyUpdate (OrderedListUpdate update))
    -> Model (WholeUpdate t)
    -> View Widget
createComboBox itemsModel whichModel = do
    esrc <- newEditSource
    rec
        store <- listStoreView blockSignal itemsModel esrc
        (blockSignal, w) <- cboxFromStore whichModel esrc store
    return w
