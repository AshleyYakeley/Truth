module Changes.UI.GTK.Table
    ( TableCellProps(..)
    , plainTableCellProps
    , KeyColumn(..)
    , readOnlyKeyColumn
    , createListTable
    ) where

import Changes.Core
import Changes.GI
import Changes.UI.GTK.DynamicStore
import Changes.UI.GTK.TextStyle
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
    , kcContents :: Model update -> GView 'Locked (Model (WholeUpdate Text), Model (ROWUpdate TableCellProps))
    }

readOnlyKeyColumn ::
       forall update.
       Model (ROWUpdate Text)
    -> (Model update -> GView 'Locked (Model (ROWUpdate (Text, TableCellProps))))
    -> KeyColumn update
readOnlyKeyColumn kcName getter = let
    kcContents :: Model update -> GView 'Locked (Model (WholeUpdate Text), Model (ROWUpdate TableCellProps))
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
       TreeView -> DynamicStore (StoreEntry update rowtext rowprops) -> Column (rowtext, rowprops) -> GView 'Locked ()
addColumn tview store col = do
    renderer <- gvNew CellRendererText []
    column <- gvNew TreeViewColumn []
    gvBindReadOnlyWholeModel (colName col) $ \t -> gvLiftIO $ #setTitle column t
    #packStart column renderer False
    cellLayoutSetAttributes column renderer (getDynamicSeqStore store) $ \entry ->
        cellAttributes col $ dynamicStoreEntryValue entry
    _ <- #appendColumn tview column
    return ()

data KeyColumns update =
    forall rowprops rowtext. MkKeyColumns (Model update -> GView 'Locked ( Model (WholeUpdate rowtext)
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
    -> GView 'Locked (Widget, Maybe (ReadM (UpdateReader update) Bool) -> GView 'Locked ())
tableContainerView (MkKeyColumns (colfunc :: Model update -> GView 'Locked ( Model (WholeUpdate rowtext)
                                                                           , Model (ROWUpdate rowprops))) cols) tableModel onActivate notifier = do
    let
        defStoreEntry :: StoreEntry update rowtext rowprops
        defStoreEntry = MkStoreEntry (error "unset model") (error "unset text") (error "unset props")
        makeStoreEntry ::
               SequencePoint
            -> ((StoreEntry update rowtext rowprops -> StoreEntry update rowtext rowprops) -> IO ())
            -> GView 'Locked ()
        makeStoreEntry i setval = do
            usub <-
                gvLiftViewNoUI $
                viewFloatMapModel
                    (changeLensToFloating (mustExistOneChangeLens "GTK table view") . orderedListItemLens i)
                    tableModel
            liftIO $ setval $ \entry -> entry {entryModel = usub}
            (textModel, propModel) <- colfunc usub
            gvBindWholeModel textModel Nothing $ \t -> gvLiftIO $ setval $ \entry -> entry {entryRowText = t}
            gvBindReadOnlyWholeModel propModel $ \t -> gvLiftIO $ setval $ \entry -> entry {entryRowProps = t}
        initTable :: GView 'Locked (DynamicStore (StoreEntry update rowtext rowprops), TreeView)
        initTable = do
            initialRows <-
                gvRunResourceContext tableModel $ \unlift amodel -> do
                    n <- liftIO $ unlift $ aModelRead amodel ListReadLength
                    return $ fmap makeStoreEntry [0 .. pred n]
            store <- newDynamicStore defStoreEntry initialRows
            tview <- treeViewNewWithModel $ getDynamicSeqStore store
            gvAcquire tview
            for_ cols $ addColumn tview store
            return (store, tview)
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
            getSelectedEntry :: GView 'Locked (Maybe (Unique, StoreEntry update rowtext rowprops))
            getSelectedEntry = do
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
                   HasCallStack
                => (DynamicStore (StoreEntry update rowtext rowprops), TreeView)
                -> NonEmpty (OrderedListUpdate update)
                -> GView 'Unlocked ()
            recvTable _ updates =
                gvRunLocked $ do
                    mselentry <- getSelectedEntry
                    withSignalBlocked tselection getSelSig $
                        for_ updates $ \case
                            OrderedListUpdateItem a b _ -> dynamicStoreMove a b store
                            OrderedListUpdateDelete i -> dynamicStoreDelete i store
                            OrderedListUpdateInsert i _ -> dynamicStoreInsert i defStoreEntry (makeStoreEntry i) store
                            OrderedListUpdateClear -> dynamicStoreClear store
                    mi <-
                        case mselentry of
                            Nothing -> return Nothing
                            Just (selu, _) -> dynamicStoreLookup selu store
                    case mi of
                        Nothing -> setSelectedIndex mi
                        Just _ -> withSignalBlocked tselection getSelSig $ setSelectedIndex mi
        (store, tview) <- gvBindModel tableModel Nothing initTable mempty recvTable
        tselection <- #getSelection tview
        set tselection [#mode := SelectionModeSingle] -- 0 or 1 selected
        let
            getSelection :: GView 'Unlocked (Maybe (Model update))
            getSelection = do
                mentry <- gvRunLocked getSelectedEntry
                return $ fmap (entryModel . snd) mentry
        MkWMFunction unliftToView <- gvGetUnliftToView
        getSelSig <-
            gvOnSignal tselection #changed $
            gvRunUnlocked $ gvLiftView $ runSelectNotify notifier $ unliftToView getSelection
    let
        setSelection :: Maybe (ReadM (UpdateReader update) Bool) -> GView 'Locked ()
        setSelection Nothing = setSelectedIndex Nothing
        setSelection (Just sel) = do
            gvLiftViewNoUI $ viewWaitUpdates tableModel
            items <- dynamicStoreContents store
            let
                testEntry :: StoreEntry update rowtext rowprops -> GView 'Locked Bool
                testEntry se =
                    gvRunResourceContext (entryModel se) $ \unlift (amod :: _ tt) ->
                        unReadM sel $ \rt -> liftIO $ unlift $ aModelRead amod rt
            mi <- mFindIndex testEntry items
            withSignalBlocked tselection getSelSig $ setSelectedIndex mi
    _ <-
        gvOnSignal tview #rowActivated $ \tpath _ -> do
            mentry <- getEntryFromPath tpath
            case mentry of
                Just (_, entry) -> onActivate $ entryModel entry
                Nothing -> return ()
    w <- toWidget tview
    return (w, setSelection)

createListTable ::
       forall update. (IsUpdate update, ApplicableEdit (UpdateEdit update), FullSubjectReader (UpdateReader update))
    => [KeyColumn update]
    -> Model (OrderedListUpdate update)
    -> (Model update -> GView 'Locked ())
    -> SelectNotify (Model update)
    -> GView 'Locked (Widget, Maybe (ReadM (UpdateReader update) Bool) -> GView 'Locked ())
createListTable cols sub onActivate sel = tableContainerView (mconcat $ fmap oneKeyColumn cols) sub onActivate sel
