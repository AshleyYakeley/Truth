module Truth.UI.GTK.Table
    ( tableGetView
    ) where

import Data.GI.Base.Attributes hiding (get)
import Data.GI.Gtk hiding (get)
import GI.Gdk hiding (get)
import GI.Gtk as Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.DynamicStore
import Truth.UI.GTK.GView
import Truth.UI.GTK.TextStyle
import Truth.UI.GTK.Useful

data Column row = MkColumn
    { colName :: Subscriber (ROWUpdate Text)
    , colText :: row -> Text
    , colProps :: row -> TableCellProps
    }

mapColumn :: (r2 -> r1) -> Column r1 -> Column r2
mapColumn f (MkColumn n t p) = MkColumn n (t . f) (p . f)

data StoreEntry update rowtext rowprops = MkStoreEntry
    { entrySubscriber :: Subscriber update
    , entryRowText :: rowtext
    , entryRowProps :: rowprops
    }

cellAttributes :: Column (rowtext, rowprops) -> StoreEntry update rowtext rowprops -> [AttrOp CellRendererText 'AttrSet]
cellAttributes MkColumn {..} MkStoreEntry {..} = let
    entryRow = (entryRowText, entryRowProps)
    MkTableCellProps {..} = colProps entryRow
    in textCellAttributes (colText entryRow) tcStyle

addColumn ::
       TreeView -> DynamicStore (StoreEntry update rowtext rowprops) -> Column (rowtext, rowprops) -> CreateView ()
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
                                                                           , Subscriber (ROWUpdate rowprops))) cols) tableSub onDoubleClick sel = do
    let
        defStoreEntry :: StoreEntry update rowtext rowprops
        defStoreEntry = MkStoreEntry (error "unset subscriber") (error "unset text") (error "unset props")
        makeStoreEntry ::
               SequencePoint seq
            -> ((StoreEntry update rowtext rowprops -> StoreEntry update rowtext rowprops) -> IO ())
            -> CreateView ()
        makeStoreEntry i setval = do
            usub <-
                cvFloatMapSubscriber
                    (editLensToFloating (mustExistOneEditLens "GTK table view") . orderedListItemLens i)
                    tableSub
            liftIO $ setval $ \entry -> entry {entrySubscriber = usub}
            (textModel, propModel) <- colfunc usub
            cvBindWholeSubscriber textModel Nothing $ \t -> liftIO $ setval $ \entry -> entry {entryRowText = t}
            cvBindReadOnlyWholeSubscriber propModel $ \t -> liftIO $ setval $ \entry -> entry {entryRowProps = t}
        initTable ::
               Subscriber (OrderedListUpdate seq update)
            -> CreateView (DynamicStore (StoreEntry update rowtext rowprops), TreeView)
        initTable osub = do
            initialRows <-
                viewRunResourceContext osub $ \unlift amodel -> do
                    n <- liftIO $ unlift $ subRead amodel ListReadLength
                    return $ fmap makeStoreEntry [0 .. pred n]
            store <- newDynamicStore defStoreEntry initialRows
            tview <- treeViewNewWithModel $ getDynamicSeqStore store
            for_ cols $ addColumn tview store
            return (store, tview)
        recvTable ::
               (DynamicStore (StoreEntry update rowtext rowprops), TreeView)
            -> NonEmpty (OrderedListUpdate seq update)
            -> View ()
        recvTable (store, _tview) updates =
            for_ updates $ \case
                OrderedListUpdateItem a b _
                    | a == b -> return ()
                OrderedListUpdateItem a b _ -> dynamicStoreMove a b store
                OrderedListUpdateDelete i -> dynamicStoreDelete i store
                OrderedListUpdateInsert i _ -> dynamicStoreInsert i defStoreEntry (makeStoreEntry i) store
                OrderedListUpdateClear -> dynamicStoreClear store
    (store, tview) <- cvBindSubscriber tableSub Nothing initTable mempty recvTable
    let
        getSelection :: View (Maybe (Subscriber update))
        getSelection = do
            tsel <- #getSelection tview
            (ltpath, _) <- #getSelectedRows tsel
            case ltpath of
                [tpath] -> do
                    ii <- #getIndices tpath
                    case ii of
                        Just [i] -> do
                            entry <- dynamicStoreGet i store
                            return $ Just $ entrySubscriber entry
                        _ -> return Nothing
                _ -> return Nothing
    _ <- cvOn tview #cursorChanged $ runSelectNotify sel getSelection
    _ <-
        cvOn tview #buttonPressEvent $ \event -> do
            click <- Gtk.get event #type
            case click of
                EventType2buttonPress -> do
                    mkey <- getSelection
                    case mkey of
                        Just key -> onDoubleClick key
                        Nothing -> return ()
                    return True
                _ -> return False
    toWidget tview

tableGetView :: GetGView
tableGetView =
    MkGetView $ \_getview uispec -> do
        MkTableUISpec cols sub onDoubleClick sel <- isUISpec uispec
        return $ keyContainerView (mconcat $ fmap oneKeyColumn cols) sub onDoubleClick sel
