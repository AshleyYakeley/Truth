module Truth.UI.GTK.Table
    ( tableGetView
    ) where

import Data.GI.Base.Attributes hiding (get)
import Data.GI.Gtk hiding (get)

--import GI.Gdk hiding (get)
--import GI.Gtk as Gtk hiding (get)
import Shapes
import Truth.Core
import Truth.UI.GTK.DynamicStore
import Truth.UI.GTK.GView
import Truth.UI.GTK.TextStyle

--import Truth.UI.GTK.Useful
data Column row = MkColumn
    { colName :: Subscriber (ROWUpdate Text)
    , colText :: row -> Text
    , colProps :: row -> TableCellProps
    }

mapColumn :: (r2 -> r1) -> Column r1 -> Column r2
mapColumn f (MkColumn n t p) = MkColumn n (t . f) (p . f)

data StoreEntry rowtext rowprops = MkStoreEntry
    { entryRowText :: rowtext
    , entryRowProps :: rowprops
    }

cellAttributes :: Column (rowtext, rowprops) -> StoreEntry rowtext rowprops -> [AttrOp CellRendererText 'AttrSet]
cellAttributes MkColumn {..} MkStoreEntry {..} = let
    entryRow = (entryRowText, entryRowProps)
    MkTableCellProps {..} = colProps entryRow
    in textCellAttributes (colText entryRow) tcStyle

addColumn :: TreeView -> DynamicStore (StoreEntry rowtext rowprops) -> Column (rowtext, rowprops) -> CreateView ()
addColumn tview store col = do
    renderer <- new CellRendererText []
    column <- new TreeViewColumn []
    cvBindReadOnlyWholeSubscriber (colName col) $ #setTitle column
    #packStart column renderer False
    cellLayoutSetAttributes column renderer (getDynamicSeqStore store) $ \entry ->
        cellAttributes col $ dynamicStoreEntryValue entry
    _ <- #appendColumn tview column
    return ()

data KeyColumns update =
    forall rowprops rowtext. MkKeyColumns (Subscriber update -> CreateView ( Subscriber (WholeUpdate rowtext)
                                                                           , Subscriber (ROWUpdate rowprops)))
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
                     ( mapSubscriber convertEditLens $ pairSubscribers lens1 lens2
                     , mapSubscriber convertReadOnlyEditLens $ pairReadOnlySubscribers func1 func2)) $
        fmap (mapColumn $ \(x, y) -> (fst x, fst y)) c1 <> fmap (mapColumn $ \(x, y) -> (snd x, snd y)) c2

instance Monoid (KeyColumns update) where
    mempty = MkKeyColumns (\_ -> return (unitSubscriber, constantSubscriber ())) []
    mappend = (<>)

keyContainerView ::
       forall seq update.
       ( IsSequence seq
       , IsUpdate update
       , ApplicableEdit (UpdateEdit update)
       , FullSubjectReader (UpdateReader update)
       , UpdateSubject update ~ Element seq
       , Integral (Index seq)
       )
    => KeyColumns update
    -> Subscriber (OrderedListUpdate seq update)
    -> (Subscriber update -> View ())
    -> SelectNotify (Subscriber update)
    -> GCreateView
keyContainerView (MkKeyColumns (colfunc :: Subscriber update -> CreateView ( Subscriber (WholeUpdate rowtext)
                                                                           , Subscriber (ROWUpdate rowprops))) cols) tableSub _onDoubleClick _sel = do
    let
        makeStoreEntry ::
               SequencePoint seq
            -> ((StoreEntry rowtext rowprops -> StoreEntry rowtext rowprops) -> IO ())
            -> CreateView ()
        makeStoreEntry i setval = do
            usub <-
                cvFloatMapSubscriber
                    (editLensToFloating (mustExistOneEditLens "GTK table view") . orderedListItemLens i)
                    tableSub
            (textModel, propModel) <- colfunc usub
            cvBindWholeSubscriber textModel Nothing $ \t -> liftIO $ setval $ \entry -> entry {entryRowText = t}
            cvBindReadOnlyWholeSubscriber propModel $ \t -> liftIO $ setval $ \entry -> entry {entryRowProps = t}
        initTable ::
               Subscriber (OrderedListUpdate seq update)
            -> CreateView (DynamicStore (StoreEntry rowtext rowprops), TreeView)
        initTable osub = do
            initialRows <-
                viewRunResourceContext osub $ \unlift amodel -> do
                    n <- liftIO $ unlift $ subRead amodel ListReadLength
                    return $ fmap makeStoreEntry [0 .. pred n]
            store <- newDynamicStore initialRows
            tview <- treeViewNewWithModel $ getDynamicSeqStore store
            for_ cols $ addColumn tview store
            return (store, tview)
        recvTable ::
               (DynamicStore (StoreEntry rowtext rowprops), TreeView)
            -> NonEmpty (OrderedListUpdate seq update)
            -> View ()
        recvTable (store, _tview) updates =
            for_ updates $ \case
                OrderedListUpdateItem a b _
                    | a == b -> return ()
                OrderedListUpdateItem a b _ -> dynamicStoreMove a b store
                OrderedListUpdateDelete i -> dynamicStoreDelete i store
                OrderedListUpdateInsert i _ -> dynamicStoreInsert i (makeStoreEntry i) store
                OrderedListUpdateClear -> dynamicStoreClear store
    (_store, tview) <- cvBindSubscriber tableSub Nothing initTable mempty recvTable
    toWidget tview

{-
    let
        getStoreItem ::
               MonadUnliftIO m
            => ContainerKey cont
            -> m (ContainerKey cont, StoreEntry  o rowtext rowprops)
        getStoreItem mr key = do
            let entryOrderFunction = geto key
            (subText, subProp) <- liftIO $ colfunc key
            entryRowText <- elGet (editLensFunction entryTextModel) mr ReadWhole
            entryRowProps <- elGet entryPropModel mr ReadWhole
            return (key, MkStoreEntry {..})
    initialRows <- liftIO $ run $ do
                MkFiniteSet initialKeys <- subRead asub KeyReadKeys
                ords <-
                    for initialKeys $ \key -> do
                        o <- elGet (geto key) (subRead asub) ReadWhole
                        return (key, o)
                let initialKeys' = fmap fst $ sortBy (\(_, a) (_, b) -> order a b) ords
                for initialKeys' $ getStoreItem
    store <- seqStoreNew initialRows
    tview <- treeViewNewWithModel store
    for_ cols $ addColumn tview store
    let
        findInStore ::
               forall m. MonadIO m
            => ContainerKey cont
            -> m (Maybe Int)
        findInStore key = do
            kk <- seqStoreToList store
            return $ lookup @[(ContainerKey cont, Int)] key $ zip (fmap fst kk) [0 ..]
    cvReceiveUpdates tableSub Nothing $ \mr updates ->
            for_ updates $ \case
                KeyUpdateDelete key -> do
                    mindex <- findInStore key
                    case mindex of
                        Just i -> seqStoreRemove store $ fromIntegral i
                        Nothing -> return ()
                KeyUpdateInsertReplace item -> let
                    key = itemKey @cont item
                    in do
                           mindex <- findInStore key
                           case mindex of
                               Just _index -> return ()
                               Nothing -> do
                                   storeItem <- getStoreItem  key
                                   _ <- seqStoreAppend store storeItem
                                   return ()
                KeyUpdateClear -> seqStoreClear store
                KeyUpdateItem _ _ -> return () -- no change to the table structure
    -- do updates to the cells
    cvReceiveUpdates tableSub Nothing $ \(mr :: MutableRead m _) tupdates -> let
        changeText :: Change m (ContainerKey cont, StoreEntry  o rowtext rowprops)
        changeText =
            MkChange $ \(key, oldcol) ->
                mapUpdates (editLensFunction $ entryTextModel oldcol) mr tupdates $ \_ updates -> do
                    newrow <-
                        mutableReadToSubject $
                        applyEdits (toList $ fmap updateEdit updates) $ subjectToMutableRead $ entryRowText oldcol
                    return (key, oldcol {entryRowText = newrow})
        changeProp :: Change m (ContainerKey cont, StoreEntry  o rowtext rowprops)
        changeProp =
            MkChange $ \(key, oldcol) ->
                mapUpdates (entryPropModel oldcol) mr tupdates $ \_ updates -> do
                    newprops <-
                        mutableReadToSubject $
                        applyEdits (toList $ fmap updateEdit updates) $ subjectToMutableRead $ entryRowProps oldcol
                    return (key, oldcol {entryRowProps = newprops})
        in seqStoreTraverse_ store $ changeText <> changeProp
    let
        getSelectedKey :: IO (Maybe (ContainerKey cont))
        getSelectedKey = do
            tsel <- #getSelection tview
            (ltpath, _) <- #getSelectedRows tsel
            case ltpath of
                [tpath] -> do
                    ii <- #getIndices tpath
                    case ii of
                        Just [i] -> do
                            (key, _) <- seqStoreGetValue store i
                            return $ Just key
                        _ -> return Nothing
                _ -> return Nothing
        aspect :: Aspect (ContainerKey cont)
        aspect = liftIO getSelectedKey
    cvAddAspect aspect
    _ <-
        cvLiftView $
        liftIOView $ \unlift ->
            on tview #focus $ \_ ->
                unlift $ do
                    viewSetSelection aspect
                    return True
    _ <-
        cvLiftView $
        liftIO $
        on tview #buttonPressEvent $ \event -> do
            click <- Gtk.get event #type
            case click of
                EventType2buttonPress -> do
                    mkey <- getSelectedKey
                    case mkey of
                        Just key -> onDoubleClick key
                        Nothing -> return ()
                    return True
                _ -> return False
    toWidget tview
-}
tableGetView :: GetGView
tableGetView =
    MkGetView $ \_getview uispec -> do
        MkTableUISpec cols sub onDoubleClick sel <- isUISpec uispec
        return $ keyContainerView (mconcat $ fmap oneKeyColumn cols) sub onDoubleClick sel
