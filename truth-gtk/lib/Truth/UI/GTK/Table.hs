module Truth.UI.GTK.Table(tableGetView) where
{
    import Shapes;
    import Graphics.UI.Gtk;
    import Truth.Core;
    import Truth.UI.GTK.Useful;
    import Truth.UI.GTK.GView;


    data Column row = MkColumn
    {
        colName :: String,
        colText :: row -> String,
        colProps :: row -> TableCellProps
    };

    mapColumn :: (r2 -> r1) -> Column r1 -> Column r2;
    mapColumn f (MkColumn n t p) = MkColumn n (t . f) (p . f);

    data StoreEntry tedit rowtext rowprops = MkStoreEntry
    {
        entryTextLens :: GeneralLens tedit (WholeEdit rowtext),
        entryPropFunc :: GeneralFunction tedit (WholeEdit rowprops),
        entryRowText :: rowtext,
        entryRowProps :: rowprops
    };

    cellAttributes :: CellRendererTextClass cell => Column (rowtext,rowprops) -> StoreEntry tedit rowtext rowprops -> [AttrOp cell];
    cellAttributes MkColumn{..} MkStoreEntry{..} =  let
    {
        entryRow = (entryRowText,entryRowProps);
        MkTableCellProps{..} = colProps entryRow;
    } in [cellText := colText entryRow,cellTextStyle := if tcItalic then StyleItalic else StyleNormal];

    addColumn :: TreeView -> ListStore (key,StoreEntry tedit rowtext rowprops) -> Column (rowtext,rowprops) -> IO ();
    addColumn tview store col = do
    {
        renderer <- cellRendererTextNew;
        column <- treeViewColumnNew;
        treeViewColumnSetTitle column $ colName col;
        cellLayoutPackStart column renderer False;
        cellLayoutSetAttributes column renderer store $ \(_,entry) -> cellAttributes col entry;
        _ <- treeViewAppendColumn tview column;
        return ();
    };

    data KeyColumns tedit key = forall rowtext rowprops. MkKeyColumns (key -> IO (GeneralLens tedit (WholeEdit rowtext),GeneralFunction tedit (WholeEdit rowprops))) [Column (rowtext,rowprops)];

    oneKeyColumn :: KeyColumn tedit key -> KeyColumns tedit key;
    oneKeyColumn (MkKeyColumn n f) = MkKeyColumns f [MkColumn n fst snd];

    instance Edit tedit => Semigroup (KeyColumns tedit key) where
    {
        MkKeyColumns f1 c1 <> MkKeyColumns f2 c2 = MkKeyColumns (\k -> do
        {
            (lens1,func1) <- f1 k;
            (lens2,func2) <- f2 k;
            return $ (convertGeneralLens <.> pairJoinGeneralLenses lens1 lens2,
            convertGeneralFunction <.> pairJoinGeneralFunctions func1 func2);
        }) $ fmap (mapColumn $ \(x,y) -> (fst x,fst y)) c1 <> fmap (mapColumn $ \(x,y) -> (snd x,snd y)) c2;
    };

    instance Edit tedit => Monoid (KeyColumns tedit key) where
    {
        mempty = MkKeyColumns (\_ -> return (constGeneralLens (),constGeneralFunction ())) [];
        mappend = (<>);
    };

    keyContainerView :: forall cont tedit iedit. (IONewItemKeyContainer cont,Edit tedit,FullSubjectReader (EditReader iedit),Edit iedit,HasKeyReader cont (EditReader iedit)) =>
        KeyColumns tedit (ContainerKey cont) -> (ContainerKey cont -> Aspect tedit) -> GeneralLens tedit (KeyEdit cont iedit) -> GCreateView tedit;
    keyContainerView (MkKeyColumns (colfunc :: ContainerKey cont -> IO (GeneralLens tedit (WholeEdit rowtext),GeneralFunction tedit (WholeEdit rowprops))) cols) getaspect tableLens = do
    {
        let
        {
            getStoreItem :: IsStateIO m => MutableRead m (EditReader tedit) -> ContainerKey cont -> m (ContainerKey cont,StoreEntry tedit rowtext rowprops);
            getStoreItem mr key = do
            {
                (entryTextLens,entryPropFunc) <- liftIO $ colfunc key;
                entryRowText <- withMapMutableRead (generalLensFunction entryTextLens) mr $ \mr' -> mr' ReadWhole;
                entryRowProps <- withMapMutableRead entryPropFunc mr $ \mr' -> mr' ReadWhole;
                return (key,MkStoreEntry{..});
            };
        };
        initalRows <- liftOuter $ viewMutableRead $ \mr -> do
        {
            MkFiniteSet initialKeys <- withMapMutableRead (generalLensFunction tableLens) mr $ \mr' -> mr' KeyReadKeys;
            for initialKeys $ getStoreItem mr;
        };
        store <- liftIO $ listStoreNew initalRows;
        tview <- liftIO $ treeViewNewWithModel store;
        liftIO $ for_ cols $ addColumn tview store;

        box <- liftIO $ vBoxNew False 0;
        newButton <- liftOuter $ liftIOView $ \unlift -> makeButton "New" $ unlift $ mapViewEdit tableLens $ viewMutableEdit $ \muted -> do
        {
            item <- liftIO $ newKeyContainerItem (Proxy::Proxy cont);
            pushMutableEdit muted [KeyInsertReplaceItem item];
        };
        liftIO $ boxPackStart box newButton PackNatural 0;
        liftIO $ boxPackStart box tview PackGrow 0;

        let
        {
            findInStore :: forall m. IsStateIO m => ContainerKey cont -> m (Maybe Int);
            findInStore key = do
            {
                kk <- liftIO $ listStoreToList store;
                return $ lookup @[(ContainerKey cont,Int)] key $ zip (fmap fst kk) [0..];
            };
        };

        createViewReceiveUpdates $ \mr edits -> mapUpdates (generalLensFunction tableLens) mr edits $ \_ edits' -> for_ edits' $ \case
        {
            KeyDeleteItem key -> do
            {
                mindex <- findInStore key;
                case mindex of
                {
                    Just i -> liftIO $ listStoreRemove store i;
                    Nothing -> return ();
                };
            };
            KeyInsertReplaceItem item -> let
            {
                key = elementKey (Proxy :: Proxy cont) item;
            } in do
            {
                mindex <- findInStore key;
                case mindex of
                {
                    Just _index -> return ();
                    Nothing -> do
                    {
                        storeItem <- getStoreItem mr key;
                        _ <- liftIO $ listStoreAppend store storeItem;
                        return ();
                    };
                };
            };
            KeyClear -> liftIO $ listStoreClear store;
            KeyEditItem _ _ -> return (); -- no change to the table structure
        };

            -- do updates to the cells
        createViewReceiveUpdates $ \mr tedits -> listStoreTraverse_ store $ joinTraverse
            (\(key,oldcol) -> mapUpdates (generalLensFunction $ entryTextLens oldcol) mr tedits $ \_ edits' -> case edits' of
            {
                [] -> return Nothing;
                _ -> do
                {
                    newrow <- fromReadFunctionM (applyEdits edits') $ return $ entryRowText oldcol;
                    return $ Just (key,oldcol{entryRowText=newrow});
                };
            })
            (\(key,oldcol) -> mapUpdates (entryPropFunc oldcol) mr tedits $ \_ edits' -> case edits' of
            {
                [] -> return Nothing;
                _ -> do
                {
                    newprops <- fromReadFunctionM (applyEdits edits') $ return $ entryRowProps oldcol;
                    return $ Just (key,oldcol{entryRowProps=newprops});
                };
            });

        _ <- liftOuter $ liftIOView $ \unlift -> on tview buttonPressEvent $ do
        {
            click <- eventClick;
            case click of
            {
                DoubleClick -> do
                {
                    liftIO $ unlift viewOpenSelection;
                    return True;
                };
                _ -> return False;
            };
        };

        let
        {
            aspect :: Aspect tedit;
            aspect = do
            {
                tsel <- treeViewGetSelection tview;
                ltpath <- treeSelectionGetSelectedRows tsel;
                case ltpath of
                {
                    [[i]] -> do
                    {
                        (key,_) <- listStoreGetValue store i;
                        getaspect key;
                    };
                    _ -> return Nothing;
                };
            };
        };

        createViewAddAspect aspect;

        _ <- liftOuter $ liftIOView $ \unlift -> on box focus $ \_ -> unlift $ do
        {
            viewSetSelectedAspect aspect;
            return True;
        };
        return $ toWidget box;
    };

    tableGetView :: GetGView;
    tableGetView = MkGetView $ \_getview uispec -> do
    {
        MkUITable cols getaspect lens <- isUISpec uispec;
        return $ keyContainerView (mconcat $ fmap oneKeyColumn cols) getaspect lens;
    };
}
