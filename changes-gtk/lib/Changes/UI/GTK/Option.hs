module Changes.UI.GTK.Option
    ( ComboBoxCell(..)
    , plainComboBoxCell
    , createComboBox
    ) where

import Changes.Core
import Changes.UI.GTK.TextStyle
import Changes.UI.GTK.Useful
import Data.GI.Base.Attributes
import Data.GI.Gtk
import Shapes

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
       forall update. (ApplicableUpdate update, FullSubjectReader (UpdateReader update))
    => WMFunction View View
    -> Model (ReadOnlyUpdate (OrderedListUpdate [UpdateSubject update] update))
    -> EditSource
    -> CreateView (SeqStore (UpdateSubject update))
listStoreView (MkWMFunction blockSignal) itemsModel esrc = let
    initV :: CreateView (SeqStore (UpdateSubject update))
    initV = do
        subjectList <- viewRunResource itemsModel $ \am -> readableToSubject $ aModelRead am
        seqStoreNew subjectList
    recv ::
           SeqStore (UpdateSubject update)
        -> NonEmpty (ReadOnlyUpdate (OrderedListUpdate [UpdateSubject update] update))
        -> View ()
    recv store updates =
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

comboBoxCellAttributes :: ComboBoxCell -> [AttrOp CellRendererText 'AttrSet]
comboBoxCellAttributes MkComboBoxCell {..} = textCellAttributes cbcText cbcStyle

cboxFromStore ::
       forall t. Eq t
    => Model (WholeUpdate t)
    -> EditSource
    -> SeqStore (t, ComboBoxCell)
    -> CreateView (WMFunction View View, Widget)
cboxFromStore whichModel esrc store = do
    widget <- comboBoxNewWithModel store
    renderer <- cvNew CellRendererText []
    #packStart widget renderer False
    cellLayoutSetAttributes widget renderer store $ \(_, cell) -> comboBoxCellAttributes cell
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
        blockSignal :: forall a. View a -> View a
        blockSignal = withSignalBlocked widget changedSignal
        findN ::
               forall l. SemiSequence l
            => [Element l -> Bool]
            -> l
            -> Maybe (Element l)
        findN [] _ = empty
        findN (p:pp) fa = find p fa <|> findN pp fa
        update :: t -> View ()
        update t = do
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
    cvBindWholeModel whichModel (Just esrc) update
    w <- toWidget widget
    return (MkWMFunction blockSignal, w)

createComboBox ::
       forall update t.
       ( Eq t
       , FullSubjectReader (UpdateReader update)
       , ApplicableUpdate update
       , UpdateSubject update ~ (t, ComboBoxCell)
       )
    => Model (ReadOnlyUpdate (OrderedListUpdate [UpdateSubject update] update))
    -> Model (WholeUpdate t)
    -> CreateView Widget
createComboBox itemsModel whichModel = do
    esrc <- newEditSource
    rec
        store <- listStoreView blockSignal itemsModel esrc
        (blockSignal, w) <- cboxFromStore whichModel esrc store
    return w
