module Truth.UI.GTK.KeyContainer(keyContainerUIView) where
{
    import Shapes;
    import Graphics.UI.Gtk;
    import Truth.Core;
    import Truth.UI.GTK.GView;


    type Update m edit = MutableRead m (EditReader edit) -> [edit] -> m ();

    mapUpdate :: forall m edita editb. IsStateIO m => GeneralLens edita editb -> Update m editb -> Update m edita;
    mapUpdate (MkCloseState lens) updateB mrA editsA = editAccess (editLensFunction lens) $ StateT $ \oldstate -> do
    {
        (newstate,editsB) <- unReadable (editUpdates (editLensFunction lens) editsA oldstate) mrA;
        let
        {
            mrB :: MutableRead m (EditReader editb);
            mrB = mapMutableRead (editGet (editLensFunction lens) newstate) mrA;
        };
        updateB mrB editsB;
        return ((),newstate);
    };

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

    keyContainerView :: forall cont tedit iedit. (IONewItemKeyContainer cont,Edit tedit) =>
        KeyColumns tedit (ContainerKey cont) -> (ContainerKey cont -> Aspect tedit) -> GeneralLens tedit (KeyEdit cont iedit) -> GView tedit;
    keyContainerView (MkKeyColumns (colfunc :: ContainerKey cont -> IO (GeneralLens tedit (WholeEdit row))) cols) getaspect tableLens = MkView $ \(object :: Object tedit) setSelect -> do
    {
        let
        {
            MkObject tableObject = mapObject tableLens object;

            getStoreItem :: IsStateIO m => MutableRead m (EditReader tedit) -> ContainerKey cont -> m (ContainerKey cont,GeneralLens tedit (WholeEdit row),row);
            getStoreItem mr key = do
            {
                lens <- liftIO $ colfunc key;
                row <- withMapMutableRead lens mr $ \mr' -> mr' ReadWhole;
                return (key,lens,row);
            };
        };
        initalRows <- runObject object $ \muted -> do
        {
            MkFiniteSet initialKeys <- withMapMutableRead tableLens (mutableRead muted) $ \mr' -> mr' KeyReadKeys;
            for initialKeys $ getStoreItem $ mutableRead muted;
        };
        store <- listStoreNew initalRows;
        tview <- treeViewNewWithModel store;
        for_ cols $ \(name,showCell) -> addColumn tview store name showCell;

        box <- vBoxNew False 0;
        newButton <- makeButton "New" $ tableObject $ \muted -> do
        {
            item <- liftIO $ newKeyContainerItem (Proxy::Proxy cont);
            maction <- mutableEdit muted [KeyInsertReplaceItem item];
            case maction of
            {
                Just action -> action;
                Nothing -> return ();
            }
        };
        boxPackStart box newButton PackNatural 0;
        boxPackStart box tview PackGrow 0;

        let
        {
            vrWidget = toWidget box;

            findInStore :: forall m. IsStateIO m => ContainerKey cont -> m (Maybe Int);
            findInStore key = do
            {
                kk <- liftIO $ listStoreToList store;
                return $ lookup @[(ContainerKey cont,Int)] key $ zip (fmap (\(k,_,_) -> k) kk) [0..];
            };

            vrUpdate :: forall m. IsStateIO m => MutableRead m (EditReader tedit) -> [tedit] -> m ();
            vrUpdate mr = mapUpdate tableLens (\_ edits -> for_ edits $ \edit -> (case edit of
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
                KeyEditItem key _ -> do
                {
                    mindex <- findInStore key;
                    case mindex of
                    {
                        Just i -> do
                        {
                            (_,lens,_) <- liftIO $ listStoreGetValue store i;
                            row <- withMapMutableRead lens mr $ \mr' -> mr' ReadWhole;
                            liftIO $ listStoreSetValue store i (key,lens,row);
                            return ();
                        };
                        Nothing -> return ();
                    };
                };
            } :: m ())) mr;

            vrFirstAspectGetter :: Aspect tedit;
            vrFirstAspectGetter = do
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

        _ <- on box focus $ \_ -> do
        {
            setSelect vrFirstAspectGetter;
            return True;
        };
        return MkViewResult{..};
    };

    keyContainerUIView :: GetGView;
    keyContainerUIView = MkGetView $ \_getview uispec -> do
    {
        MkUITable cols getaspect lens <- isUISpec uispec;
        return $ keyContainerView (mconcat $ fmap oneKeyColumn cols) getaspect lens;
    };
}
