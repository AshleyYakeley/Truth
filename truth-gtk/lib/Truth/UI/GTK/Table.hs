module Truth.UI.GTK.Table
    ( tableGetView
    ) where

import Data.GI.Base.Attributes
import Data.GI.Gtk
import GI.Gdk
import GI.Gtk as Gtk
import GI.Pango
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

data Column updateT row = MkColumn
    { colName :: UpdateFunction updateT (WholeUpdate Text)
    , colText :: row -> Text
    , colProps :: row -> TableCellProps
    }

mapColumn :: (r2 -> r1) -> Column updateT r1 -> Column updateT r2
mapColumn f (MkColumn n t p) = MkColumn n (t . f) (p . f)

data StoreEntry updateT o rowtext rowprops = MkStoreEntry
    { entryOrderFunction :: UpdateFunction updateT (WholeUpdate o)
    , entryTextLens :: EditLens updateT (WholeUpdate rowtext)
    , entryPropFunc :: UpdateFunction updateT (WholeUpdate rowprops)
    , entryRowText :: rowtext
    , entryRowProps :: rowprops
    }

cellAttributes ::
       Column updateT (rowtext, rowprops) -> StoreEntry updateT o rowtext rowprops -> [AttrOp CellRendererText 'AttrSet]
cellAttributes MkColumn {..} MkStoreEntry {..} = let
    entryRow = (entryRowText, entryRowProps)
    MkTableCellProps {..} = colProps entryRow
    in [ #text := colText entryRow
       , #style :=
         if tcItalic
             then StyleItalic
             else StyleNormal
       ]

addColumn ::
       TreeView
    -> SeqStore (key, StoreEntry updateT o rowtext rowprops)
    -> Column updateT (rowtext, rowprops)
    -> CreateView key updateT ()
addColumn tview store col = do
    renderer <- new CellRendererText []
    column <- new TreeViewColumn []
    cvBindUpdateFunction Nothing (colName col) $ #setTitle column
    #packStart column renderer False
    cellLayoutSetAttributes column renderer store $ \(_, entry) -> cellAttributes col entry
    _ <- #appendColumn tview column
    return ()

data KeyColumns updateT key =
    forall rowprops rowtext. MkKeyColumns (key -> IO ( EditLens updateT (WholeUpdate rowtext)
                                                     , UpdateFunction updateT (WholeUpdate rowprops)))
                                          [Column updateT (rowtext, rowprops)]

oneKeyColumn :: KeyColumn updateT key -> KeyColumns updateT key
oneKeyColumn (MkKeyColumn n f) = MkKeyColumns f [MkColumn n fst snd]

instance Semigroup (KeyColumns updateT key) where
    MkKeyColumns f1 c1 <> MkKeyColumns f2 c2 =
        MkKeyColumns
            (\k -> do
                 (lens1, func1) <- f1 k
                 (lens2, func2) <- f2 k
                 return $
                     ( convertEditLens . pairCombineEditLenses lens1 lens2
                     , convertUpdateFunction . pairCombineUpdateFunctions func1 func2)) $
        fmap (mapColumn $ \(x, y) -> (fst x, fst y)) c1 <> fmap (mapColumn $ \(x, y) -> (snd x, snd y)) c2

instance Monoid (KeyColumns updateT key) where
    mempty = MkKeyColumns (\_ -> return (constEditLens (), constUpdateFunction ())) []
    mappend = (<>)

keyContainerView ::
       forall cont o updateT updateI.
       (KeyContainer cont, FullSubjectReader (UpdateReader updateI), HasKeyReader cont (UpdateReader updateI))
    => KeyColumns updateT (ContainerKey cont)
    -> (o -> o -> Ordering)
    -> (ContainerKey cont -> UpdateFunction updateT (WholeUpdate o))
    -> EditLens updateT (KeyUpdate cont updateI)
    -> (ContainerKey cont -> IO ())
    -> GCreateView (ContainerKey cont) updateT
keyContainerView (MkKeyColumns (colfunc :: ContainerKey cont -> IO ( EditLens updateT (WholeUpdate rowtext)
                                                                   , UpdateFunction updateT (WholeUpdate rowprops))) cols) order geto tableLens onDoubleClick = do
    let
        getStoreItem ::
               MonadUnliftIO m
            => MutableRead m (UpdateReader updateT)
            -> ContainerKey cont
            -> m (ContainerKey cont, StoreEntry updateT o rowtext rowprops)
        getStoreItem mr key = do
            let entryOrderFunction = geto key
            (entryTextLens, entryPropFunc) <- liftIO $ colfunc key
            entryRowText <- updateFunctionRead (editLensFunction entryTextLens) mr ReadWhole
            entryRowProps <- updateFunctionRead entryPropFunc mr ReadWhole
            return (key, MkStoreEntry {..})
    initialRows <-
        cvLiftView $ do
            viewObjectRead $ \_ mr -> do
                MkFiniteSet initialKeys <- updateFunctionRead (editLensFunction tableLens) mr KeyReadKeys
                ords <-
                    for initialKeys $ \key -> do
                        o <- updateFunctionRead (geto key) mr ReadWhole
                        return (key, o)
                let initialKeys' = fmap fst $ sortBy (\(_, a) (_, b) -> order a b) ords
                for initialKeys' $ getStoreItem mr
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
    cvReceiveUpdates Nothing $ \_ mr edits ->
        mapUpdates (editLensFunction tableLens) mr edits $ \_ edits' ->
            withTransConstraintTM @MonadIO $
            for_ edits' $ \case
                KeyUpdateDelete key -> do
                    mindex <- findInStore key
                    case mindex of
                        Just i -> seqStoreRemove store $ fromIntegral i
                        Nothing -> return ()
                KeyUpdateInsertReplace item -> let
                    key = elementKey @cont item
                    in do
                           mindex <- findInStore key
                           case mindex of
                               Just _index -> return ()
                               Nothing -> do
                                   storeItem <- lift $ getStoreItem mr key
                                   _ <- seqStoreAppend store storeItem
                                   return ()
                KeyUpdateClear -> seqStoreClear store
                KeyUpdateItem _ _ -> return () -- no change to the table structure
    -- do updates to the cells
    cvReceiveUpdates Nothing $ \_ (mr :: MutableRead m _) tupdates -> let
        changeText :: Change m (ContainerKey cont, StoreEntry updateT o rowtext rowprops)
        changeText =
            MkChange $ \(key, oldcol) ->
                mapUpdates (editLensFunction $ entryTextLens oldcol) mr tupdates $ \_ updates ->
                    withTransConstraintTM @MonadIO $
                    case updates of
                        [] -> return Nothing
                        _ -> do
                            newrow <-
                                mutableReadToSubject $
                                applyEdits (fmap updateEdit updates) $ subjectToMutableRead $ entryRowText oldcol
                            return $ Just (key, oldcol {entryRowText = newrow})
        changeProp :: Change m (ContainerKey cont, StoreEntry updateT o rowtext rowprops)
        changeProp =
            MkChange $ \(key, oldcol) ->
                mapUpdates (entryPropFunc oldcol) mr tupdates $ \_ updates ->
                    withTransConstraintTM @MonadIO $
                    case updates of
                        [] -> return Nothing
                        _ -> do
                            newprops <-
                                mutableReadToSubject $
                                applyEdits (fmap updateEdit updates) $ subjectToMutableRead $ entryRowProps oldcol
                            return $ Just (key, oldcol {entryRowProps = newprops})
        in seqStoreTraverse_ store $ changeText <> changeProp
    let
        aspect :: Aspect (ContainerKey cont)
        aspect = do
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
                    liftIO $ do
                        mkey <- aspect
                        case mkey of
                            Just key -> onDoubleClick key
                            Nothing -> return ()
                    return True
                _ -> return False
    toWidget tview

tableGetView :: GetGView
tableGetView =
    MkGetView $ \_getview uispec -> do
        MkTableUISpec cols order geto lens onDoubleClick <- isUISpec uispec
        return $ keyContainerView (mconcat $ fmap oneKeyColumn cols) order geto lens onDoubleClick
