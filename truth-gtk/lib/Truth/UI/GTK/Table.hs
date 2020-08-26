module Truth.UI.GTK.Table
    ( tableGetView
    ) where

import Data.GI.Base.Attributes hiding (get)
import Data.GI.Gtk hiding (get)
import Shapes
import Truth.Core
import Truth.UI.GTK.DynamicStore
import Truth.UI.GTK.GView
import Truth.UI.GTK.TextStyle
import Truth.UI.GTK.Useful

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
       TreeView -> DynamicStore (StoreEntry update rowtext rowprops) -> Column (rowtext, rowprops) -> CreateView ()
addColumn tview store col = do
    renderer <- new CellRendererText []
    column <- new TreeViewColumn []
    cvBindReadOnlyWholeModel (colName col) $ #setTitle column
    #packStart column renderer False
    cellLayoutSetAttributes column renderer (getDynamicSeqStore store) $ \entry ->
        cellAttributes col $ dynamicStoreEntryValue entry
    _ <- #appendColumn tview column
    return ()

data KeyColumns update =
    forall rowprops rowtext. MkKeyColumns (Model update -> CreateView ( Model (WholeUpdate rowtext)
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
       forall seq update.
       ( IsSequence seq
       , IsUpdate update
       , ApplicableEdit (UpdateEdit update)
       , FullSubjectReader (UpdateReader update)
       , UpdateSubject update ~ Element seq
       , Integral (Index seq)
       )
    => KeyColumns update
    -> Model (OrderedListUpdate seq update)
    -> (Model update -> View ())
    -> SelectNotify (Model update)
    -> GCreateView
tableContainerView (MkKeyColumns (colfunc :: Model update -> CreateView ( Model (WholeUpdate rowtext)
                                                                        , Model (ROWUpdate rowprops))) cols) tableSub onActivate notifier = do
    let
        defStoreEntry :: StoreEntry update rowtext rowprops
        defStoreEntry = MkStoreEntry (error "unset model") (error "unset text") (error "unset props")
        makeStoreEntry ::
               SequencePoint seq
            -> ((StoreEntry update rowtext rowprops -> StoreEntry update rowtext rowprops) -> IO ())
            -> CreateView ()
        makeStoreEntry i setval = do
            usub <-
                cvFloatMapModel
                    (changeLensToFloating (mustExistOneChangeLens "GTK table view") . orderedListItemLens i)
                    tableSub
            liftIO $ setval $ \entry -> entry {entryModel = usub}
            (textModel, propModel) <- colfunc usub
            cvBindWholeModel textModel Nothing $ \t -> liftIO $ setval $ \entry -> entry {entryRowText = t}
            cvBindReadOnlyWholeModel propModel $ \t -> liftIO $ setval $ \entry -> entry {entryRowProps = t}
        initTable ::
               Model (OrderedListUpdate seq update)
            -> CreateView (DynamicStore (StoreEntry update rowtext rowprops), TreeView)
        initTable osub = do
            initialRows <-
                viewRunResourceContext osub $ \unlift amodel -> do
                    n <- liftIO $ unlift $ aModelRead amodel ListReadLength
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
    (store, tview) <- cvBindModel tableSub Nothing initTable mempty recvTable
    tselection <- #getSelection tview
    set tselection [#mode := SelectionModeSingle] -- 0 or 1 selected
    let
        getItemFromPath :: TreePath -> View (Maybe (Model update))
        getItemFromPath tpath = do
            ii <- #getIndices tpath
            case ii of
                Just [i] -> do
                    entry <- dynamicStoreGet i store
                    return $ Just $ entryModel entry
                _ -> return Nothing
        getSelection :: View (Maybe (Model update))
        getSelection = do
            (ltpath, _) <- #getSelectedRows tselection
            case ltpath of
                [tpath] -> getItemFromPath tpath
                _ -> return Nothing
    _ <- cvOn tselection #changed $ runSelectNotify notifier getSelection
    _ <-
        cvOn tview #rowActivated $ \tpath _ -> do
            msel <- getItemFromPath tpath
            case msel of
                Just sel -> onActivate sel
                Nothing -> return ()
    toWidget tview

tableGetView :: GetGView
tableGetView =
    MkGetView $ \_getview uispec -> do
        MkTableUISpec cols sub onActivate sel <- isUISpec uispec
        return $ tableContainerView (mconcat $ fmap oneKeyColumn cols) sub onActivate sel
