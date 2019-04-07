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

data Column tedit row = MkColumn
    { colName :: EditFunction tedit (WholeEdit Text)
    , colText :: row -> Text
    , colProps :: row -> TableCellProps
    }

mapColumn :: (r2 -> r1) -> Column tedit r1 -> Column tedit r2
mapColumn f (MkColumn n t p) = MkColumn n (t . f) (p . f)

data StoreEntry tedit rowtext rowprops = MkStoreEntry
    { entryTextLens :: EditLens tedit (WholeEdit rowtext)
    , entryPropFunc :: EditFunction tedit (WholeEdit rowprops)
    , entryRowText :: rowtext
    , entryRowProps :: rowprops
    }

cellAttributes ::
       Column tedit (rowtext, rowprops) -> StoreEntry tedit rowtext rowprops -> [AttrOp CellRendererText 'AttrSet]
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
    -> SeqStore (key, StoreEntry tedit rowtext rowprops)
    -> Column tedit (rowtext, rowprops)
    -> CreateView key tedit ()
addColumn tview store col = do
    renderer <- new CellRendererText []
    column <- new TreeViewColumn []
    cvBindEditFunction Nothing (colName col) $ #setTitle column
    #packStart column renderer False
    cellLayoutSetAttributes column renderer store $ \(_, entry) -> cellAttributes col entry
    _ <- #appendColumn tview column
    return ()

data KeyColumns tedit key =
    forall rowprops rowtext. MkKeyColumns (key -> IO ( EditLens tedit (WholeEdit rowtext)
                                                     , EditFunction tedit (WholeEdit rowprops)))
                                          [Column tedit (rowtext, rowprops)]

oneKeyColumn :: KeyColumn tedit key -> KeyColumns tedit key
oneKeyColumn (MkKeyColumn n f) = MkKeyColumns f [MkColumn n fst snd]

instance Semigroup (KeyColumns tedit key) where
    MkKeyColumns f1 c1 <> MkKeyColumns f2 c2 =
        MkKeyColumns
            (\k -> do
                 (lens1, func1) <- f1 k
                 (lens2, func2) <- f2 k
                 return $
                     ( convertEditLens . pairCombineEditLenses lens1 lens2
                     , convertEditFunction . pairCombineEditFunctions func1 func2)) $
        fmap (mapColumn $ \(x, y) -> (fst x, fst y)) c1 <> fmap (mapColumn $ \(x, y) -> (snd x, snd y)) c2

instance Monoid (KeyColumns tedit key) where
    mempty = MkKeyColumns (\_ -> return (constEditLens (), constEditFunction ())) []
    mappend = (<>)

keyContainerView ::
       forall cont tedit iedit.
       (KeyContainer cont, FullSubjectReader (EditReader iedit), HasKeyReader cont (EditReader iedit))
    => KeyColumns tedit (ContainerKey cont)
    -> EditLens tedit (KeyEdit cont iedit)
    -> (ContainerKey cont -> IO ())
    -> GCreateView (ContainerKey cont) tedit
keyContainerView (MkKeyColumns (colfunc :: ContainerKey cont -> IO ( EditLens tedit (WholeEdit rowtext)
                                                                   , EditFunction tedit (WholeEdit rowprops))) cols) tableLens onDoubleClick = do
    let
        getStoreItem ::
               MonadUnliftIO m
            => MutableRead m (EditReader tedit)
            -> ContainerKey cont
            -> m (ContainerKey cont, StoreEntry tedit rowtext rowprops)
        getStoreItem mr key = do
            (entryTextLens, entryPropFunc) <- liftIO $ colfunc key
            entryRowText <- editFunctionRead (editLensFunction entryTextLens) mr ReadWhole
            entryRowProps <- editFunctionRead entryPropFunc mr ReadWhole
            return (key, MkStoreEntry {..})
    initialRows <-
        cvLiftView $
        viewObjectRead $ \_ mr -> do
            MkFiniteSet initialKeys <- editFunctionRead (editLensFunction tableLens) mr KeyReadKeys
            for initialKeys $ getStoreItem mr
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
                KeyDeleteItem key -> do
                    mindex <- findInStore key
                    case mindex of
                        Just i -> seqStoreRemove store $ fromIntegral i
                        Nothing -> return ()
                KeyInsertReplaceItem item -> let
                    key = elementKey @cont item
                    in do
                           mindex <- findInStore key
                           case mindex of
                               Just _index -> return ()
                               Nothing -> do
                                   storeItem <- lift $ getStoreItem mr key
                                   _ <- seqStoreAppend store storeItem
                                   return ()
                KeyClear -> seqStoreClear store
                KeyEditItem _ _ -> return () -- no change to the table structure
    -- do updates to the cells
    cvReceiveUpdates Nothing $ \_ mr tedits ->
        seqStoreTraverse_ store $
        joinTraverse
            (\(key, oldcol) ->
                 mapUpdates (editLensFunction $ entryTextLens oldcol) mr tedits $ \_ edits' ->
                     withTransConstraintTM @MonadIO $
                     case edits' of
                         [] -> return Nothing
                         _ -> do
                             newrow <-
                                 mutableReadToSubject $ applyEdits edits' $ subjectToMutableRead $ entryRowText oldcol
                             return $ Just (key, oldcol {entryRowText = newrow}))
            (\(key, oldcol) ->
                 mapUpdates (entryPropFunc oldcol) mr tedits $ \_ edits' ->
                     withTransConstraintTM @MonadIO $
                     case edits' of
                         [] -> return Nothing
                         _ -> do
                             newprops <-
                                 mutableReadToSubject $ applyEdits edits' $ subjectToMutableRead $ entryRowProps oldcol
                             return $ Just (key, oldcol {entryRowProps = newprops}))
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
        MkTableUISpec cols lens onDoubleClick <- isUISpec uispec
        return $ keyContainerView (mconcat $ fmap oneKeyColumn cols) lens onDoubleClick
