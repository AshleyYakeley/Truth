module Changes.World.GNOME.GTK.Widget.Table
    ( TableCellProps(..)
    , plainTableCellProps
    , KeyColumn(..)
    , readOnlyKeyColumn
    , createListTable
    ) where

import Changes.Core
import Changes.World.GNOME.GI
import Changes.World.GNOME.GTK.Widget.DynamicStore
import Changes.World.GNOME.GTK.Widget.TextStyle
import Data.GI.Base.Attributes hiding (get)
import Data.GI.Gtk hiding (get)
import Shapes

data TableCellProps = MkTableCellProps
    { tcStyle :: TextStyle
    }

plainTableCellProps :: TableCellProps
plainTableCellProps = let
    tcStyle = plainTextStyle
    in MkTableCellProps {..}

data KeyColumn update = MkKeyColumn
    { kcName :: Model (ROWUpdate Text)
    , kcContents :: Model update -> GView 'Unlocked (Model (WholeUpdate Text), Model (ROWUpdate TableCellProps))
    }

readOnlyKeyColumn ::
       forall update.
       Model (ROWUpdate Text)
    -> (Model update -> GView 'Unlocked (Model (ROWUpdate (Text, TableCellProps))))
    -> KeyColumn update
readOnlyKeyColumn kcName getter = let
    kcContents :: Model update -> GView 'Unlocked (Model (WholeUpdate Text), Model (ROWUpdate TableCellProps))
    kcContents rowSub = do
        cellSub <- getter rowSub
        let
            textSub :: Model (WholeUpdate Text)
            textSub = mapModel (fromReadOnlyRejectingChangeLens . liftReadOnlyChangeLens (funcChangeLens fst)) cellSub
            propsSub :: Model (ROWUpdate TableCellProps)
            propsSub = mapModel (liftReadOnlyChangeLens $ funcChangeLens snd) cellSub
        return (textSub, propsSub)
    in MkKeyColumn {..}

data Column row = MkColumn
    { colName :: Model (ROWUpdate Text)
    , colText :: row -> Text
    , colProps :: row -> TableCellProps
    }

mapColumn :: (r2 -> r1) -> Column r1 -> Column r2
mapColumn f (MkColumn n t p) = MkColumn n (t . f) (p . f)

data StoreEntry update rowtext rowprops = MkStoreEntry
    { entryModel :: Model update
    , entryRowText :: rowtext
    , entryRowProps :: rowprops
    }

cellAttributes :: Column (rowtext, rowprops) -> StoreEntry update rowtext rowprops -> [AttrOp CellRendererText 'AttrSet]
cellAttributes MkColumn {..} MkStoreEntry {..} = let
    entryRow = (entryRowText, entryRowProps)
    MkTableCellProps {..} = colProps entryRow
    in textCellAttributes (colText entryRow) tcStyle

addColumn ::
       TreeView -> DynamicStore (StoreEntry update rowtext rowprops) -> Column (rowtext, rowprops) -> GView 'Unlocked ()
addColumn tview store col = do
    gvRunLockedThen $ do
        renderer <- gvNew CellRendererText []
        column <- gvNew TreeViewColumn []
        #packStart column renderer False
        cellLayoutSetAttributes column renderer (getDynamicSeqStore store) $ \entry ->
            cellAttributes col $ dynamicStoreEntryValue entry
        _ <- #appendColumn tview column
        return $ do gvBindReadOnlyWholeModel (colName col) $ \t -> gvRunLocked $ #setTitle column t

data KeyColumns update =
    forall rowprops rowtext. MkKeyColumns (Model update -> GView 'Unlocked ( Model (WholeUpdate rowtext)
                                                                           , Model (ROWUpdate rowprops)))
                                          [Column (rowtext, rowprops)]

oneKeyColumn :: KeyColumn update -> KeyColumns update
oneKeyColumn (MkKeyColumn n f) = MkKeyColumns f [MkColumn n fst snd]

instance Semigroup (KeyColumns update) where
    MkKeyColumns f1 c1 <> MkKeyColumns f2 c2 =
        MkKeyColumns
            (\k -> do
                 (lens1, func1) <- f1 k
                 (lens2, func2) <- f2 k
                 return $
                     ( mapModel convertChangeLens $ pairModels lens1 lens2
                     , mapModel convertReadOnlyChangeLens $ pairReadOnlyModels func1 func2)) $
        fmap (mapColumn $ \(x, y) -> (fst x, fst y)) c1 <> fmap (mapColumn $ \(x, y) -> (snd x, snd y)) c2

instance Monoid (KeyColumns update) where
    mempty = MkKeyColumns (\_ -> return (unitModel, constantModel ())) []
    mappend = (<>)

tableContainerView ::
       forall update.
       (HasCallStack, IsUpdate update, ApplicableEdit (UpdateEdit update), FullSubjectReader (UpdateReader update))
    => KeyColumns update
    -> Model (OrderedListUpdate update)
    -> (Model update -> GView 'Locked ())
    -> SelectNotify (Model update)
    -> GView 'Unlocked (Widget, Maybe (ReadM (UpdateReader update) Bool) -> GView 'Unlocked ())
tableContainerView (MkKeyColumns (colfunc :: Model update -> GView 'Unlocked ( Model (WholeUpdate rowtext)
                                                                             , Model (ROWUpdate rowprops))) cols) tableModel onActivate notifier = do
    let
        defStoreEntry :: StoreEntry update rowtext rowprops
        defStoreEntry = MkStoreEntry (error "unset model") (error "unset text") (error "unset props")
        makeStoreEntry ::
               SequencePoint
            -> ((StoreEntry update rowtext rowprops -> StoreEntry update rowtext rowprops) -> GView 'Locked ())
            -> GView 'Unlocked ()
        makeStoreEntry i setval = do
            usub <-
                gvLiftView $
                viewFloatMapModel
                    (changeLensToFloating (mustExistOneChangeLens "GTK table view") . orderedListItemLens i)
                    tableModel
            gvRunLocked $ setval $ \entry -> entry {entryModel = usub}
            (textModel, propModel) <- colfunc usub
            gvBindWholeModel textModel Nothing $ \t -> gvRunLocked $ setval $ \entry -> entry {entryRowText = t}
            gvBindReadOnlyWholeModel propModel $ \t -> gvRunLocked $ setval $ \entry -> entry {entryRowProps = t}
        initTable :: GView 'Unlocked (DynamicStore (StoreEntry update rowtext rowprops), TreeView, Widget)
        initTable = do
            initialRows <-
                gvLiftView $
                viewRunResourceContext tableModel $ \unlift amodel -> do
                    n <- liftIO $ unlift $ aModelRead amodel ListReadLength
                    return $ fmap makeStoreEntry [0 .. pred n]
            store <- newDynamicStore defStoreEntry initialRows
            gvRunLockedThen $ do
                tview <- treeViewNewWithModel $ getDynamicSeqStore store
                widget <- toWidget tview
                gvAcquire tview
                return $ do
                    for_ cols $ addColumn tview store
                    return (store, tview, widget)
    rec
        let
            getEntryFromPath :: TreePath -> GView 'Locked (Maybe (Unique, StoreEntry update rowtext rowprops))
            getEntryFromPath tpath = do
                ii <- #getIndices tpath
                case ii of
                    Just [i] -> do
                        uentry <- dynamicStoreGet i store
                        return $ Just uentry
                    _ -> return Nothing
            getSelectedEntry :: GView 'Unlocked (Maybe (Unique, StoreEntry update rowtext rowprops))
            getSelectedEntry =
                gvRunLocked $ do
                    (ltpath, _) <- #getSelectedRows tselection
                    case ltpath of
                        [tpath] -> getEntryFromPath tpath
                        _ -> return Nothing
            setSelectedIndex :: Maybe Int -> GView 'Locked ()
            setSelectedIndex Nothing = #unselectAll tselection
            setSelectedIndex (Just i) = do
                tpath <- treePathNewFromIndices [fromIntegral i]
                #selectPath tselection tpath
            recvTable ::
                   (DynamicStore (StoreEntry update rowtext rowprops), TreeView, Widget)
                -> NonEmpty (OrderedListUpdate update)
                -> GView 'Unlocked ()
            recvTable _ updates = do
                mselentry <- getSelectedEntry
                withSignalBlocked tselection getSelSig $
                    for_ updates $ \case
                        OrderedListUpdateItem a b _ -> dynamicStoreMove a b store
                        OrderedListUpdateDelete i -> dynamicStoreDelete i store
                        OrderedListUpdateInsert i _ -> dynamicStoreInsert i defStoreEntry (makeStoreEntry i) store
                        OrderedListUpdateClear -> dynamicStoreClear store
                gvRunLocked $ do
                    mi <-
                        case mselentry of
                            Nothing -> return Nothing
                            Just (selu, _) -> dynamicStoreLookup selu store
                    case mi of
                        Nothing -> setSelectedIndex mi
                        Just _ -> withSignalBlocked tselection getSelSig $ setSelectedIndex mi
        (store, tview, widget) <- gvBindModel tableModel Nothing initTable mempty recvTable
        (getSelSig, tselection) <-
            gvRunLocked $ do
                tselection' <- #getSelection tview
                set tselection' [#mode := SelectionModeSingle] -- 0 or 1 selected
                let
                    getSelection :: GView 'Unlocked (Maybe (Model update))
                    getSelection = do
                        mentry <- getSelectedEntry
                        return $ fmap (entryModel . snd) mentry
                MkWRaised unliftToView <- gvGetUnliftToView
                getSelSig' <-
                    gvOnSignal tselection' #changed $
                    gvRunUnlocked $ gvLiftView $ runSelectNotify notifier $ unliftToView getSelection
                return (getSelSig', tselection')
    let
        setSelection :: Maybe (ReadM (UpdateReader update) Bool) -> GView 'Unlocked ()
        setSelection Nothing = gvRunLocked $ setSelectedIndex Nothing
        setSelection (Just sel) = do
            gvLiftView $ viewWaitUpdates tableModel
            items <- gvRunLocked $ dynamicStoreContents store
            let
                testEntry :: StoreEntry update rowtext rowprops -> GView 'Unlocked Bool
                testEntry se =
                    gvLiftView $
                    viewRunResourceContext (entryModel se) $ \unlift (amod :: _ tt) ->
                        unReadM sel $ \rt -> liftIO $ unlift $ aModelRead amod rt
            mi <- mFindIndex testEntry items
            withSignalBlocked tselection getSelSig $ gvRunLocked $ setSelectedIndex mi
    _ <-
        gvRunLocked $
        gvOnSignal tview #rowActivated $ \tpath _ -> do
            mentry <- getEntryFromPath tpath
            case mentry of
                Just (_, entry) -> onActivate $ entryModel entry
                Nothing -> return ()
    return (widget, setSelection)

createListTable ::
       forall update. (IsUpdate update, ApplicableEdit (UpdateEdit update), FullSubjectReader (UpdateReader update))
    => [KeyColumn update]
    -> Model (OrderedListUpdate update)
    -> (Model update -> GView 'Locked ())
    -> SelectNotify (Model update)
    -> GView 'Unlocked (Widget, Maybe (ReadM (UpdateReader update) Bool) -> GView 'Unlocked ())
createListTable cols sub onActivate sel = tableContainerView (concatmap oneKeyColumn cols) sub onActivate sel
