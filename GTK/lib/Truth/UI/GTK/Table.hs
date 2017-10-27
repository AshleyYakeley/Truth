module Truth.UI.GTK.Table(tableGetView) where
{
    import Shapes;
    import Graphics.UI.Gtk;
    import Truth.Core;
    import Truth.UI.GTK.Useful;
    import Truth.UI.GTK.GView;


    addColumn :: TreeView -> ListStore (key,GeneralLens tedit (WholeEdit row),row) -> String -> (row -> String) -> IO ();
    addColumn tview store name showCell = do
    {
        renderer <- cellRendererTextNew;
        column <- treeViewColumnNew;
        treeViewColumnSetTitle column name;
        cellLayoutPackStart column renderer False;
        cellLayoutSetAttributes column renderer store $ \(_,_,row) -> [cellText := showCell row];
        _ <- treeViewAppendColumn tview column;
        return ();
    };

    data KeyColumns tedit key = forall row. MkKeyColumns (key -> IO (GeneralLens tedit (WholeEdit row))) [(String,row -> String)];

    oneKeyColumn :: KeyColumn tedit key -> KeyColumns tedit key;
    oneKeyColumn (MkKeyColumn n f) = MkKeyColumns f [(n,id)];

    instance Edit tedit => Semigroup (KeyColumns tedit key) where
    {
        MkKeyColumns f1 c1 <> MkKeyColumns f2 c2 = MkKeyColumns (\k -> do
        {
            lens1 <- f1 k;
            lens2 <- f2 k;
            return $ convertGeneralLens <.> pairJoinGeneralLenses lens1 lens2;
        }) $ fmap (\(n,l) -> (n,l . fst)) c1 <> fmap (\(n,l) -> (n,l . snd)) c2;
    };

    instance Edit tedit => Monoid (KeyColumns tedit key) where
    {
        mempty = MkKeyColumns (\_ -> return $ constGeneralLens ()) [];
        mappend = (<>);
    };

    keyContainerView :: forall cont tedit iedit. (IONewItemKeyContainer cont,Edit tedit,FullSubjectReader (EditReader iedit),Edit iedit,HasKeyReader cont (EditReader iedit)) =>
        KeyColumns tedit (ContainerKey cont) -> (ContainerKey cont -> Aspect tedit) -> GeneralLens tedit (KeyEdit cont iedit) -> GCreateView tedit;
    keyContainerView (MkKeyColumns (colfunc :: ContainerKey cont -> IO (GeneralLens tedit (WholeEdit row))) cols) getaspect tableLens = do
    {
        let
        {
            getStoreItem :: IsStateIO m => MutableRead m (EditReader tedit) -> ContainerKey cont -> m (ContainerKey cont,GeneralLens tedit (WholeEdit row),row);
            getStoreItem mr key = do
            {
                lens <- liftIO $ colfunc key;
                row <- withMapMutableRead lens mr $ \mr' -> mr' ReadWhole;
                return (key,lens,row);
            };
        };
        initalRows <- liftOuter $ viewMutableRead $ \mr -> do
        {
            MkFiniteSet initialKeys <- withMapMutableRead tableLens mr $ \mr' -> mr' KeyReadKeys;
            for initialKeys $ getStoreItem mr;
        };
        store <- liftIO $ listStoreNew initalRows;
        tview <- liftIO $ treeViewNewWithModel store;
        liftIO $ for_ cols $ \(name,showCell) -> addColumn tview store name showCell;

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
                return $ lookup @[(ContainerKey cont,Int)] key $ zip (fmap (\(k,_,_) -> k) kk) [0..];
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

        createViewReceiveUpdates $ \mr tedits -> do
        {
            -- do updates to the cells
            listStoreTraverse_ store $ \(key,lens,oldrow) -> mapUpdates (generalLensFunction lens) mr tedits $ \_ edits' -> case edits' of
            {
                [] -> return Nothing;
                _ -> do
                {
                    newrow <- fromReadFunctionM (applyEdits edits') (return oldrow);
                    return $ Just (key,lens,newrow);
                };
            };
        };

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
                        (key,_,_) <- listStoreGetValue store i;
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
