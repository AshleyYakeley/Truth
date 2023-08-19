module Changes.World.GNOME.GTK.Element.Option
    ( ComboBoxCell(..)
    , plainComboBoxCell
    , createComboBox
    ) where

import Changes.Core
import Changes.World.GNOME.GI
import Changes.World.GNOME.GTK.Element.TextStyle
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
       forall update. (IsEditUpdate update, ApplicableEdit (UpdateEdit update), FullSubjectReader (UpdateReader update))
    => WRaised (GView 'Locked) (GView 'Locked)
    -> Model (ReadOnlyUpdate (OrderedListUpdate update))
    -> EditSource
    -> GView 'Unlocked (SeqStore (UpdateSubject update))
listStoreView (MkWRaised blockSignal) itemsModel esrc = let
    initV :: GView 'Unlocked (SeqStore (UpdateSubject update))
    initV = do
        subjects <- gvLiftView $ viewRunResource itemsModel $ \am -> readableToSubject $ aModelRead am
        gvRunLocked $ seqStoreNew $ toList subjects
    recv ::
           SeqStore (UpdateSubject update) -> NonEmpty (ReadOnlyUpdate (OrderedListUpdate update)) -> GView 'Unlocked ()
    recv store updates =
        for_ updates $ \(MkReadOnlyUpdate lupdate) ->
            case lupdate of
                OrderedListUpdateItem oldi newi []
                    | oldi == newi -> return ()
                OrderedListUpdateItem (fromIntegral -> oldi) (fromIntegral -> newi) iupdates ->
                    gvRunLocked $ do
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
                OrderedListUpdateDelete (fromIntegral -> i) -> gvRunLocked $ blockSignal $ seqStoreRemove store i
                OrderedListUpdateInsert (fromIntegral -> i) item ->
                    gvRunLocked $ blockSignal $ seqStoreInsert store i item
                OrderedListUpdateClear -> gvRunLocked $ blockSignal $ seqStoreClear store
    in gvBindModel itemsModel (Just esrc) initV mempty recv

comboBoxCellAttributes :: ComboBoxCell -> [AttrOp CellRendererText 'AttrSet]
comboBoxCellAttributes MkComboBoxCell {..} = textCellAttributes cbcText cbcStyle

cboxFromStore ::
       forall t. Eq t
    => Model (WholeUpdate t)
    -> EditSource
    -> SeqStore (t, ComboBoxCell)
    -> GView 'Unlocked (WRaised (GView 'Locked) (GView 'Locked), Widget)
cboxFromStore whichModel esrc store =
    gvRunLockedThen $ do
        cbox <- comboBoxNewWithModel store
        widget <- toWidget cbox
        renderer <- gvNew CellRendererText []
        #packStart cbox renderer False
        cellLayoutSetAttributes cbox renderer store $ \(_, cell) -> comboBoxCellAttributes cell
        changedSignal <-
            gvOnSignal cbox #changed $ do
                mi <- #getActiveIter cbox
                case mi of
                    (True, iter) -> do
                        i <- seqStoreIterToIndex iter
                        (t, _) <- seqStoreGetValue store i
                        _ <-
                            gvRunUnlocked $
                            gvLiftView $
                            viewRunResource whichModel $ \asub ->
                                pushEdit esrc $ aModelEdit asub $ pure $ MkWholeReaderEdit t
                        return ()
                    (False, _) -> return ()
        let
            blockSignal :: GView 'Locked --> GView 'Locked
            blockSignal = withSignalBlocked cbox changedSignal
            findN ::
                   forall l. SemiSequence l
                => [Element l -> Bool]
                -> l
                -> Maybe (Element l)
            findN [] _ = empty
            findN (p:pp) fa = find p fa <|> findN pp fa
            update :: t -> GView 'Unlocked ()
            update t =
                gvRunLocked $ do
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
                                Just ti -> blockSignal $ #setActiveIter cbox $ Just ti
                                Nothing -> return ()
                        Nothing -> return ()
        return $ do
            gvBindWholeModel whichModel (Just esrc) update
            return (MkWRaised blockSignal, widget)

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
    -> GView 'Unlocked Widget
createComboBox itemsModel whichModel = do
    esrc <- gvNewEditSource
    rec
        store <- listStoreView blockSignal itemsModel esrc
        (blockSignal, w) <- cboxFromStore whichModel esrc store
    return w
