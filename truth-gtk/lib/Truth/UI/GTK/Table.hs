module Truth.UI.GTK.Table
    ( tableGetView
    ) where

import Shapes
import Truth.UI.GTK.GView

{-
import Data.GI.Base.Attributes hiding (get)
import Data.GI.Gtk hiding (get)
import GI.Gdk hiding (get)
import GI.Gtk as Gtk hiding (get)
import Truth.Core
import Truth.UI.GTK.TextStyle
import Truth.UI.GTK.Useful

{-
data KeyColumn update = MkKeyColumn
    { kcName :: OpenSubscriber (ROWUpdate Text)
    , kcContents :: OpenSubscriber update -> IO ( OpenSubscriber (WholeUpdate Text)
                                                , OpenSubscriber (ROWUpdate TableCellProps))
    }
-}

data Column  row = MkColumn
    { colName :: OpenSubscriber (ROWUpdate Text)
    , colText :: row -> Text
    , colProps :: row -> TableCellProps
    }

mapColumn :: (r2 -> r1) -> Column  r1 -> Column  r2
mapColumn f (MkColumn n t p) = MkColumn n (t . f) (p . f)

data StoreEntry update rowtext rowprops = MkStoreEntry
    { entryTextLens :: OpenSubscriber  (WholeUpdate rowtext)
    , entryPropFunc :: OpenSubscriber (ROWUpdate rowprops)
    , entryRowText :: rowtext
    , entryRowProps :: rowprops
    , entryViewState :: ViewState (Subscriber update)
    }

cellAttributes ::
       Column  (rowtext, rowprops) -> StoreEntry update   rowtext rowprops -> [AttrOp CellRendererText 'AttrSet]
cellAttributes MkColumn {..} MkStoreEntry {..} = let
    entryRow = (entryRowText, entryRowProps)
    MkTableCellProps {..} = colProps entryRow
    in textCellAttributes (colText entryRow) tcStyle

addColumn ::
       TreeView
    -> SeqStore (StoreEntry update  rowtext rowprops)
    -> Column  (rowtext, rowprops)
    -> CreateView sel  ()
addColumn tview store col = do
    renderer <- new CellRendererText []
    column <- new TreeViewColumn []
    cvBindReadOnlyWholeSubscriber (colName col) $ #setTitle column
    #packStart column renderer False
    cellLayoutSetAttributes column renderer store $ \entry -> cellAttributes col entry
    _ <- #appendColumn tview column
    return ()

data KeyColumns  update =
    forall rowprops rowtext. MkKeyColumns (OpenSubscriber update -> IO ( OpenSubscriber  (WholeUpdate rowtext)
                                                     , OpenSubscriber (ROWUpdate rowprops)))
                                          [Column  (rowtext, rowprops)]

oneKeyColumn :: KeyColumn  update -> KeyColumns  update
oneKeyColumn (MkKeyColumn n f) = MkKeyColumns f [MkColumn n fst snd]

instance Semigroup (KeyColumns  update) where
    MkKeyColumns f1 c1 <> MkKeyColumns f2 c2 =
        MkKeyColumns
            (\k -> do
                 (lens1, func1) <- f1 k
                 (lens2, func2) <- f2 k
                 return $
                     (mapSubscriber convertEditLens $ pairSubscribers lens1 lens2
                     , mapReadOnlySubscriber_convertUpdateFunction_pairReadOnlySubscribers func1 func2)) $
        fmap (mapColumn $ \(x, y) -> (fst x, fst y)) c1 <> fmap (mapColumn $ \(x, y) -> (snd x, snd y)) c2

instance Monoid (KeyColumns  update) where
    mempty = MkKeyColumns (\_ -> return (openResource unitSubscriber, openResource $ constantSubscriber ())) []
    mappend = (<>)

keyContainerView :: forall seq update. (SubjectReader (UpdateReader update), Integral (Index seq)) =>
    KeyColumns update -> OpenSubscriber (OrderedListUpdate seq update) -> (OpenSubscriber update -> IO ()) -> GCreateView (Subscriber update)
{-
keyContainerView ::
       forall cont o  updateI.
       (FullSubjectReader (UpdateReader updateI), HasKeyReader cont (UpdateReader updateI))
    => KeyColumns  (ContainerKey cont)
    -> (o -> o -> Ordering)
    -> (ContainerKey cont -> Subscriber  (ROWUpdate o))
    -> Subscriber  (KeyUpdate cont updateI)
    -> (ContainerKey cont -> IO ())
    -> GCreateView (ContainerKey cont)
-}

{-
orderedListItemLens ::
       forall seq update.
       ( IsSequence seq
       , FullSubjectReader (UpdateReader update)
       , ApplicableEdit (UpdateEdit update)
       , UpdateSubject update ~ Element seq
       )
    => SequencePoint seq
    -> FloatingEditLens (OrderedListUpdate seq update) (MaybeUpdate update)
-}

keyContainerView (MkKeyColumns (colfunc :: OpenSubscriber update -> IO ( OpenSubscriber  (WholeUpdate rowtext)
                                                                   , OpenSubscriber (ROWUpdate rowprops))) cols) tableSub onDoubleClick = do
    let
        makeStoreEntry :: SequencePoint seq -> IO (StoreEntry update rowtext rowprops)
        makeStoreEntry i = do
            usub <- floatMapOpenSubscriber (orderedListItemLens i) tableSub
            (textSub, propsub) <- colfunc usub
            return MkStoreEntry {..}
        initTable :: OpenSubscriber (OrderedListUpdate seq update) -> CreateView sel (SeqStore (StoreEntry update rowtext rowprops), TreeView)
        initTable osub = do
            initialRows <- liftIO $ withOpenResource osub $ \asub -> do
                n <- subRead asub ListReadLength
                liftIO $ for [0 .. pred n] makeStoreEntry
            store <- seqStoreNew initialRows
            tview <- treeViewNewWithModel store
            for_ cols $ addColumn tview store
            return (store,tview)
        recvTable :: (SeqStore (StoreEntry update rowtext rowprops), TreeView) -> NonEmpty (OrderedListUpdate seq update) -> IO ()
        recvTable (store,tview) updates = for_ updates $ \case
            OrderedListUpdateItem a b _ | a == b -> return ()
            OrderedListUpdateItem a b _ -> do
                entry <- seqStoreGetValue store $ fromIntegral a
                seqStoreRemove store $ fromIntegral a
                seqStoreInsert store (fromIntegral b) entry
            OrderedListUpdateDelete i -> do
                entry <- seqStoreGetValue store $ fromIntegral i
                closeLifeState $ entryViewState entry
                seqStoreRemove store $ fromIntegral i
            OrderedListUpdateInsert i _ -> do
                entry <- makeStoreEntry i
                seqStoreInsert store (fromIntegral i) entry
            OrderedListUpdateClear -> do
                entries <- seqStoreToList store
                for_ entries $ \entry -> closeLifeState $ entryViewState entry
                seqStoreClear store

    (_store,tview) <- cvBindSubscriber tableSub Nothing initTable mempty recvTable
    toWidget tview

{-
    let
        getStoreItem ::
               MonadUnliftIO m
            => ContainerKey cont
            -> m (ContainerKey cont, StoreEntry update  o rowtext rowprops)
        getStoreItem mr key = do
            let entryOrderFunction = geto key
            (subText, subProp) <- liftIO $ colfunc key
            entryRowText <- elGet (editLensFunction entryTextLens) mr ReadWhole
            entryRowProps <- elGet entryPropFunc mr ReadWhole
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
        changeText :: Change m (ContainerKey cont, StoreEntry update  o rowtext rowprops)
        changeText =
            MkChange $ \(key, oldcol) ->
                mapUpdates (editLensFunction $ entryTextLens oldcol) mr tupdates $ \_ updates -> do
                    newrow <-
                        mutableReadToSubject $
                        applyEdits (toList $ fmap updateEdit updates) $ subjectToMutableRead $ entryRowText oldcol
                    return (key, oldcol {entryRowText = newrow})
        changeProp :: Change m (ContainerKey cont, StoreEntry update  o rowtext rowprops)
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
-}
tableGetView :: GetGView
tableGetView = mempty
{-
tableGetView =
    MkGetView $ \_getview uispec -> do
        MkTableUISpec cols sub onDoubleClick <- isUISpec uispec
        return $ keyContainerView (mconcat $ fmap oneKeyColumn cols) sub onDoubleClick
-}
