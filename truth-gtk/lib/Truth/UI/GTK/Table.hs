module Truth.UI.GTK.Table
    ( tableGetView
    ) where

import Shapes
import Truth.UI.GTK.GView

{-
import Data.GI.Base.Attributes
import Data.GI.Gtk
import GI.Gdk
import GI.Gtk as Gtk
import Truth.Core
import Truth.UI.GTK.TextStyle
import Truth.UI.GTK.Useful

data Column  row = MkColumn
    { colName :: ReadOnlySubscriber (WholeUpdate Text)
    , colText :: row -> Text
    , colProps :: row -> TableCellProps
    }

mapColumn :: (r2 -> r1) -> Column  r1 -> Column  r2
mapColumn f (MkColumn n t p) = MkColumn n (t . f) (p . f)

data StoreEntry  o rowtext rowprops = MkStoreEntry
    { entryOrderFunction :: ReadOnlySubscriber (WholeUpdate o)
    , entryTextLens :: Subscriber  (WholeUpdate rowtext)
    , entryPropFunc :: ReadOnlySubscriber (WholeUpdate rowprops)
    , entryRowText :: rowtext
    , entryRowProps :: rowprops
    }

cellAttributes ::
       Column  (rowtext, rowprops) -> StoreEntry  o rowtext rowprops -> [AttrOp CellRendererText 'AttrSet]
cellAttributes MkColumn {..} MkStoreEntry {..} = let
    entryRow = (entryRowText, entryRowProps)
    MkTableCellProps {..} = colProps entryRow
    in textCellAttributes (colText entryRow) tcStyle

addColumn ::
       TreeView
    -> SeqStore (key, StoreEntry  o rowtext rowprops)
    -> Column  (rowtext, rowprops)
    -> CreateView key  ()
addColumn tview store col = do
    renderer <- new CellRendererText []
    column <- new TreeViewColumn []
    cvBindUpdateFunction Nothing (colName col) $ #setTitle column
    #packStart column renderer False
    cellLayoutSetAttributes column renderer store $ \(_, entry) -> cellAttributes col entry
    _ <- #appendColumn tview column
    return ()

data KeyColumns  key =
    forall rowprops rowtext. MkKeyColumns (key -> IO ( Subscriber  (WholeUpdate rowtext)
                                                     , ReadOnlySubscriber (WholeUpdate rowprops)))
                                          [Column  (rowtext, rowprops)]

oneKeyColumn :: KeyColumn  key -> KeyColumns  key
oneKeyColumn (MkKeyColumn n f) = MkKeyColumns f [MkColumn n fst snd]

instance Semigroup (KeyColumns  key) where
    MkKeyColumns f1 c1 <> MkKeyColumns f2 c2 =
        MkKeyColumns
            (\k -> do
                 (lens1, func1) <- f1 k
                 (lens2, func2) <- f2 k
                 return $
                     (mapPureSubscriber convertEditLens $ pairSubscribers lens1 lens2
                     , mapReadOnlySubscriber convertUpdateFunction $ pairReadOnlySubscribers func1 func2)) $
        fmap (mapColumn $ \(x, y) -> (fst x, fst y)) c1 <> fmap (mapColumn $ \(x, y) -> (snd x, snd y)) c2

instance Monoid (KeyColumns  key) where
    mempty = MkKeyColumns (\_ -> return (unitSubscriber, constantSubscriber ())) []
    mappend = (<>)


keyContainerView ::
       forall cont o  updateI.
       (KeyContainer cont, FullSubjectReader (UpdateReader updateI), HasKeyReader cont (UpdateReader updateI))
    => KeyColumns  (ContainerKey cont)
    -> (o -> o -> Ordering)
    -> (ContainerKey cont -> ReadOnlySubscriber  (WholeUpdate o))
    -> Subscriber  (KeyUpdate cont updateI)
    -> (ContainerKey cont -> IO ())
    -> GCreateView (ContainerKey cont)
keyContainerView (MkKeyColumns (colfunc :: ContainerKey cont -> IO ( Subscriber  (WholeUpdate rowtext)
                                                                   , ReadOnlySubscriber (WholeUpdate rowprops))) cols) order geto tableSub onDoubleClick = runResource tableSub $ \run asub -> do
    let
        getStoreItem ::
               MonadUnliftIO m
            => ContainerKey cont
            -> m (ContainerKey cont, StoreEntry  o rowtext rowprops)
        getStoreItem mr key = do
            let entryOrderFunction = geto key
            (subText, subProp) <- liftIO $ colfunc key
            entryRowText <- ufGet (editLensFunction entryTextLens) mr ReadWhole
            entryRowProps <- ufGet entryPropFunc mr ReadWhole
            return (key, MkStoreEntry {..})
    initialRows <- liftIO $ run $ do
                MkFiniteSet initialKeys <- subRead asub KeyReadKeys
                ords <-
                    for initialKeys $ \key -> do
                        o <- ufGet (geto key) (subRead asub) ReadWhole
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
    cvReceiveUpdates tableSub Nothing $ \_ mr updates ->
            for_ updates $ \case
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
                                   storeItem <- getStoreItem  key
                                   _ <- seqStoreAppend store storeItem
                                   return ()
                KeyUpdateClear -> seqStoreClear store
                KeyUpdateItem _ _ -> return () -- no change to the table structure
    -- do updates to the cells
    cvReceiveUpdates tableSub Nothing $ \_ (mr :: MutableRead m _) tupdates -> let
        changeText :: Change m (ContainerKey cont, StoreEntry  o rowtext rowprops)
        changeText =
            MkChange $ \(key, oldcol) ->
                mapUpdates (editLensFunction $ entryTextLens oldcol) mr tupdates $ \_ updates -> do
                    newrow <-
                        mutableReadToSubject $
                        applyEdits (toList $ fmap updateEdit updates) $ subjectToMutableRead $ entryRowText oldcol
                    return (key, oldcol {entryRowText = newrow})
        changeProp :: Change m (ContainerKey cont, StoreEntry  o rowtext rowprops)
        changeProp =
            MkChange $ \(key, oldcol) ->
                mapUpdates (entryPropFunc oldcol) mr tupdates $ \_ updates -> do
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
tableGetView = mempty
{-
tableGetView =
    MkGetView $ \_getview uispec -> do
        MkTableUISpec cols order geto lens onDoubleClick <- isUISpec uispec
        return $ keyContainerView (mconcat $ fmap oneKeyColumn cols) order geto lens onDoubleClick
-}
