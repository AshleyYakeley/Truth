module Changes.World.GNOME.GTK.Widget.Table
    ( TableCellProps (..)
    , plainTableCellProps
    , KeyColumn (..)
    , readOnlyKeyColumn
    , createListTable
    )
where

import Data.IORef

import Changes.World.GNOME.GI
import Changes.World.GNOME.GTK.Widget.DynamicStore
import Changes.World.GNOME.GTK.Widget.TextStyle
import Import
import Import.GI qualified as GI

data TableCellProps = MkTableCellProps
    { tcStyle :: TextStyle
    }

plainTableCellProps :: TableCellProps
plainTableCellProps =
    let
        tcStyle = plainTextStyle
        in MkTableCellProps{..}

data KeyColumn update = MkKeyColumn
    { kcName :: Model (ROWUpdate Text)
    , kcContents :: Model update -> GView 'Unlocked (Model (WholeUpdate Text), Model (ROWUpdate TableCellProps))
    }

readOnlyKeyColumn ::
    forall update.
    Model (ROWUpdate Text) ->
    (Model update -> GView 'Unlocked (Model (ROWUpdate (Text, TableCellProps)))) ->
    KeyColumn update
readOnlyKeyColumn kcName getter =
    let
        kcContents :: Model update -> GView 'Unlocked (Model (WholeUpdate Text), Model (ROWUpdate TableCellProps))
        kcContents rowSub = do
            cellSub <- getter rowSub
            let
                textSub :: Model (WholeUpdate Text)
                textSub = mapModel (fromReadOnlyRejectingChangeLens . liftReadOnlyChangeLens (funcChangeLens fst)) cellSub
                propsSub :: Model (ROWUpdate TableCellProps)
                propsSub = mapModel (liftReadOnlyChangeLens $ funcChangeLens snd) cellSub
            return (textSub, propsSub)
        in MkKeyColumn{..}

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

setCellLabel ::
    GI.Label ->
    Column (rowtext, rowprops) ->
    StoreEntry update rowtext rowprops ->
    GSemiview 'Locked ()
setCellLabel label MkColumn{..} MkStoreEntry{..} = do
    let
        entryRow = (entryRowText, entryRowProps)
        MkTableCellProps{..} = colProps entryRow
        text = colText entryRow
    if tsItalic tcStyle
        then do
            escapedText <- liftIO $ GI.markupEscapeText text (-1)
            liftIO $ GI.labelSetMarkup label $ "<i>" <> escapedText <> "</i>"
        else liftIO $ GI.labelSetText label text

setListItemCell ::
    Column (rowtext, rowprops) ->
    GI.ListItem ->
    StoreEntry update rowtext rowprops ->
    GSemiview 'Locked ()
setListItemCell col item entry = do
    mchild <- liftIO $ GI.listItemGetChild item
    for_ mchild $ \child -> do
        mlabel <- liftIO $ GI.castTo GI.Label child
        for_ mlabel $ \label -> setCellLabel label col entry

sameListItem :: GI.ListItem -> GI.ListItem -> IO Bool
sameListItem a b = do
    aptr <- GI.unsafeManagedPtrGetPtr a
    bptr <- GI.unsafeManagedPtrGetPtr b
    return $ aptr == bptr

addColumn ::
    GI.ColumnView ->
    DynamicStore (StoreEntry update rowtext rowprops) ->
    Column (rowtext, rowprops) ->
    GView 'Unlocked ()
addColumn tview store col = do
    gvRunLockedThen $ do
        activeItems <- gvLiftIO $ newIORef []
        factory <- gvLiftIO GI.signalListItemFactoryNew
        gvBind factory
        _ <-
            gvOnSignal () factory #setup $ \object -> do
                mitem <- gvLiftIO $ GI.castTo GI.ListItem object
                for_ mitem $ \item -> do
                    label <- gvNew GI.Label [#hexpand GI.:= True, #xalign GI.:= 0]
                    gvLiftIO $ GI.listItemSetChild item $ Just label
                    gvLiftIO $ modifyIORef' activeItems (item :)
        _ <-
            gvOnSignal () factory #bind $ \object -> do
                mitem <- gvLiftIO $ GI.castTo GI.ListItem object
                for_ mitem $ \item -> do
                    position <- gvLiftIO $ GI.listItemGetPosition item
                    (_, entry) <- lift $ dynamicStoreGet position store
                    lift $ setListItemCell col item entry
        _ <-
            gvOnSignal () factory #teardown $ \object -> do
                mitem <- gvLiftIO $ GI.castTo GI.ListItem object
                for_ mitem $ \item -> do
                    items <- gvLiftIO $ readIORef activeItems
                    remaining <- gvLiftIO $ filterM (fmap not . sameListItem item) items
                    gvLiftIO $ writeIORef activeItems remaining
        dynamicStoreOnChange store $ \position entry -> do
            items <- gsvLiftIOTrustMeNoUI $ readIORef activeItems
            for_ items $ \item -> do
                itemPosition <- liftIO $ GI.listItemGetPosition item
                when (itemPosition == fromIntegral position) $ setListItemCell col item entry
        factoryTransferred <- gvDuplicateUnbound GI.SignalListItemFactory factory
        column <- gvLiftIO $ GI.columnViewColumnNew Nothing $ Just factoryTransferred
        gvBind column
        gvLiftIO $ GI.columnViewColumnSetExpand column True
        gvLiftIO $ GI.columnViewAppendColumn tview column
        return
            $ gvBindReadOnlyWholeModel (colName col)
            $ \title -> gvRunLocked $ gvLiftIO $ GI.columnViewColumnSetTitle column $ Just title

data KeyColumns update
    = forall rowprops rowtext. MkKeyColumns
        ( Model update ->
          GView
            'Unlocked
            ( Model (WholeUpdate rowtext)
            , Model (ROWUpdate rowprops)
            )
        )
        [Column (rowtext, rowprops)]

oneKeyColumn :: KeyColumn update -> KeyColumns update
oneKeyColumn (MkKeyColumn n f) = MkKeyColumns f [MkColumn n fst snd]

instance Semigroup (KeyColumns update) where
    MkKeyColumns f1 c1 <> MkKeyColumns f2 c2 =
        MkKeyColumns
            ( \k -> do
                (lens1, func1) <- f1 k
                (lens2, func2) <- f2 k
                return
                    $ ( mapModel convertChangeLens $ pairModels lens1 lens2
                      , mapModel convertReadOnlyChangeLens $ pairReadOnlyModels func1 func2
                      )
            )
            $ fmap (mapColumn $ \(x, y) -> (fst x, fst y)) c1
            <> fmap (mapColumn $ \(x, y) -> (snd x, snd y)) c2

instance Monoid (KeyColumns update) where
    mempty = MkKeyColumns (\_ -> return (unitModel, constantModel ())) []
    mappend = (<>)

tableContainerView ::
    forall update.
    (HasCallStack, IsUpdate update, ApplicableEdit (UpdateEdit update), FullSubjectReader (UpdateReader update)) =>
    KeyColumns update ->
    Model (OrderedListUpdate update) ->
    (Model update -> GView 'Locked ()) ->
    SelectNotify (Model update) ->
    GView 'Unlocked (GI.Widget, Maybe (ReadM (UpdateReader update) Bool) -> GView 'Unlocked ())
tableContainerView
    ( MkKeyColumns
            ( colfunc ::
                    Model update ->
                    GView
                        'Unlocked
                        ( Model (WholeUpdate rowtext)
                        , Model (ROWUpdate rowprops)
                        )
                )
            cols
        )
    tableModel
    onActivate
    notifier = do
        let
            defStoreEntry :: StoreEntry update rowtext rowprops
            defStoreEntry = MkStoreEntry (error "unset model") (error "unset text") (error "unset props")
            makeStoreEntry ::
                SequencePoint ->
                ((StoreEntry update rowtext rowprops -> StoreEntry update rowtext rowprops) -> GSemiview 'Locked ()) ->
                GView 'Unlocked ()
            makeStoreEntry i setval = do
                usub <-
                    gvLiftView
                        $ viewFloatMapModel
                            (changeLensToFloating (mustExistOneChangeLens "GTK table view") . orderedListItemLens i)
                            tableModel
                gvRunLocked $ lift $ setval $ \entry -> entry{entryModel = usub}
                (textModel, propModel) <- colfunc usub
                gvBindWholeModel textModel Nothing $ \t -> gvRunLocked $ lift $ setval $ \entry -> entry{entryRowText = t}
                gvBindReadOnlyWholeModel propModel $ \t -> gvRunLocked $ lift $ setval $ \entry -> entry{entryRowProps = t}
            initTable ::
                GView
                    'Unlocked
                    ( DynamicStore (StoreEntry update rowtext rowprops)
                    , GI.ColumnView
                    , GI.SingleSelection
                    , GI.Widget
                    )
            initTable = do
                initialRows <-
                    gvLiftView
                        $ viewRunResourceContext tableModel
                        $ \unlift amodel -> do
                            n <- liftIO $ unlift $ aModelRead amodel ListReadLength
                            return $ fmap makeStoreEntry [0 .. pred n]
                store <- newDynamicStore defStoreEntry initialRows
                gvRunLockedThen $ do
                    listModelTransferred <- gvDuplicateUnbound GI.StringList $ getDynamicListModel store
                    tselection <- gvLiftIO $ GI.singleSelectionNew $ Just listModelTransferred
                    gvBind tselection
                    gvLiftIO $ GI.singleSelectionSetAutoselect tselection False
                    gvLiftIO $ GI.singleSelectionSetCanUnselect tselection True
                    gvLiftIO $ GI.singleSelectionSetSelected tselection GI.INVALID_LIST_POSITION
                    selectionTransferred <- gvDuplicateUnbound GI.SingleSelection tselection
                    tview <- gvLiftIO $ GI.columnViewNew $ Just selectionTransferred
                    gvBind tview
                    widget <- gvLiftIO $ GI.toWidget tview
                    return $ do
                        for_ cols $ addColumn tview store
                        return (store, tview, tselection, widget)
        rec let
                getSelectedEntry :: GView 'Unlocked (Maybe (Unique, StoreEntry update rowtext rowprops))
                getSelectedEntry =
                    gvRunLocked $ do
                        position <- gvLiftIO $ GI.singleSelectionGetSelected tselection
                        if position == GI.INVALID_LIST_POSITION
                            then return Nothing
                            else Just <$> lift (dynamicStoreGet position store)
                setSelectedIndex :: Maybe Int -> GView 'Locked ()
                setSelectedIndex Nothing =
                    gvLiftIO $ GI.singleSelectionSetSelected tselection GI.INVALID_LIST_POSITION
                setSelectedIndex (Just i) =
                    gvLiftIO $ GI.singleSelectionSetSelected tselection $ fromIntegral i
                recvTable ::
                    ( DynamicStore (StoreEntry update rowtext rowprops)
                    , GI.ColumnView
                    , GI.SingleSelection
                    , GI.Widget
                    ) ->
                    NonEmpty (OrderedListUpdate update) ->
                    GView 'Unlocked ()
                recvTable _ updates = do
                    mselentry <- getSelectedEntry
                    withSignalBlocked tselection getSelSig
                        $ for_ updates
                        $ \case
                            OrderedListUpdateItem a b _ -> lift $ dynamicStoreMove a b store
                            OrderedListUpdateDelete i -> lift $ dynamicStoreDelete i store
                            OrderedListUpdateInsert i _ -> lift $ dynamicStoreInsert i defStoreEntry (makeStoreEntry i) store
                            OrderedListUpdateClear -> lift $ dynamicStoreClear store
                    gvRunLocked $ do
                        mi <-
                            case mselentry of
                                Nothing -> return Nothing
                                Just (selu, _) -> lift $ dynamicStoreLookup selu store
                        case mi of
                            Nothing -> setSelectedIndex mi
                            Just _ -> withSignalBlocked tselection getSelSig $ setSelectedIndex mi
            (store, tview, tselection, widget) <- gvBindModel tableModel Nothing initTable mempty recvTable
            getSelSig <-
                gvRunLocked $ do
                    let
                        getSelection :: GView 'Unlocked (Maybe (Model update))
                        getSelection = do
                            mentry <- getSelectedEntry
                            return $ fmap (entryModel . snd) mentry
                    gvOnSignal () tselection #selectionChanged $ \_ _ ->
                        gvRunUnlocked $ do
                            selection <- getSelection
                            gvLiftView $ runSelectNotify notifier $ return selection
        let
            setSelection :: Maybe (ReadM (UpdateReader update) Bool) -> GView 'Unlocked ()
            setSelection Nothing = gvRunLocked $ setSelectedIndex Nothing
            setSelection (Just sel) = do
                gvLiftView $ viewWaitUpdates tableModel
                items <- gvRunLocked $ lift $ dynamicStoreContents store
                let
                    testEntry :: StoreEntry update rowtext rowprops -> GView 'Unlocked Bool
                    testEntry se =
                        gvLiftView
                            $ viewRunResourceContext (entryModel se)
                            $ \unlift (amod :: _ tt) ->
                                unReadM sel $ \rt -> liftIO $ unlift $ aModelRead amod rt
                mi <- mFindIndex testEntry items
                withSignalBlocked tselection getSelSig $ gvRunLocked $ setSelectedIndex mi
        _ <-
            gvRunLocked
                $ gvOnSignal () tview #activate
                $ \position -> do
                    (_, entry) <- lift $ dynamicStoreGet position store
                    onActivate $ entryModel entry
        return (widget, setSelection)

createListTable ::
    forall update.
    (IsUpdate update, ApplicableEdit (UpdateEdit update), FullSubjectReader (UpdateReader update)) =>
    [KeyColumn update] ->
    Model (OrderedListUpdate update) ->
    (Model update -> GView 'Locked ()) ->
    SelectNotify (Model update) ->
    GView 'Unlocked (GI.Widget, Maybe (ReadM (UpdateReader update) Bool) -> GView 'Unlocked ())
createListTable cols sub onActivate sel = tableContainerView (concatmap oneKeyColumn cols) sub onActivate sel
