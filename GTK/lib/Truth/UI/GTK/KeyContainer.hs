module Truth.UI.GTK.KeyContainer(keyContainerUIView) where
{
    import Shapes;
    import Graphics.UI.Gtk;
    import Truth.Core;
    import Truth.UI.GTK.GView;


    data SALens edita editb = forall state. MkSALens (forall m. IsStateIO m => StateAccess m state) (EditLens state edita editb);

    makeSALens :: GeneralLens edita editb -> IO (SALens edita editb);
    makeSALens (MkCloseState lens) = do
    {
        var <- newMVar $ editInitial $ editLensFunction lens;
        return $ MkSALens (mvarStateAccess var) lens;
    };

    saMapObject :: Edit edita => SALens edita editb -> Object edita -> Object editb;
    saMapObject (MkSALens sa lens) = mapObject sa lens;

    type Update m edit = MutableRead m (EditReader edit) -> [edit] -> m ();
    saMapUpdate :: forall m edita editb. IsStateIO m => SALens edita editb -> Update m editb -> Update m edita;
    saMapUpdate (MkSALens sa lens) updateB mrA editsA = sa $ StateT $ \oldstate -> do
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

    addColumn :: Edit tedit => TreeView -> Object tedit -> ListStore (key,SALens tedit (WholeEdit row)) -> String -> (row -> String) -> IO ();
    addColumn tview object store name showCell = do
    {
        renderer <- cellRendererTextNew;
        column <- treeViewColumnNew;
        treeViewColumnSetTitle column name;
        cellLayoutPackStart column renderer False;
        cellLayoutSetAttributes column renderer store $ \(_,lens) -> [cellText :=> do
        {
            row <- runObject (saMapObject lens object) $ \muted -> mutableRead muted ReadWhole;
            return $ showCell row;
        }];
        _ <- treeViewAppendColumn tview column;
        return ();
    };

    data KeyColumns tedit key = forall row. MkKeyColumns (key -> GeneralLens tedit (WholeEdit row)) [(String,row -> String)];

    oneKeyColumn :: KeyColumn tedit key -> KeyColumns tedit key;
    oneKeyColumn (MkKeyColumn n f) = MkKeyColumns f [(n,id)];

    instance Edit tedit => Semigroup (KeyColumns tedit key) where
    {
        MkKeyColumns f1 c1 <> MkKeyColumns f2 c2 = MkKeyColumns (\k -> convertGeneralLens <.> pairJoinGeneralLenses (f1 k) (f2 k)) $ fmap (\(n,l) -> (n,l . fst)) c1 <> fmap (\(n,l) -> (n,l . snd)) c2;
    };

    instance Edit tedit => Monoid (KeyColumns tedit key) where
    {
        mempty = MkKeyColumns (\_ -> constGeneralLens ()) [];
        mappend = (<>);
    };

    keyContainerView :: forall cont tedit iedit. (IONewItemKeyContainer cont,Edit tedit) =>
        KeyColumns tedit (ContainerKey cont) -> (ContainerKey cont -> Aspect tedit) -> GeneralLens tedit (KeyEdit cont iedit) -> GView tedit;
    keyContainerView (MkKeyColumns (colfunc :: ContainerKey cont -> GeneralLens tedit (WholeEdit row)) cols) getaspect tableLens = MkView $ \(object :: Object tedit) setSelect -> do
    {
        tableSALens <- makeSALens tableLens;
        let
        {
            MkObject tableObject = saMapObject tableSALens object;
        };
        MkFiniteSet initialKeys <- tableObject $ \muted -> mutableRead muted KeyReadKeys;
        initialRows <- for initialKeys $ \key -> do
        {
            rowSALens <- liftIO $ makeSALens $ colfunc key;
            return (key,rowSALens);
        };
        store <- listStoreNew initialRows;
        tview <- treeViewNewWithModel store;
        for_ cols $ \(name,showCell) -> addColumn tview object store name showCell;

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
                return $ lookup @[(ContainerKey cont,Int)] key $ zip (fmap fst kk) [0..];
            };

            vrUpdate :: forall m. IsStateIO m => MutableRead m (EditReader tedit) -> [tedit] -> m ();
            vrUpdate = saMapUpdate tableSALens $ \_mr edits -> for_ edits $ \edit -> (case edit of
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
                            rowSALens <- liftIO $ makeSALens $ colfunc key;
                            _ <- liftIO $ listStoreAppend store (key,rowSALens);
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
                            (_,lens) <- liftIO $ listStoreGetValue store i;
                            liftIO $ listStoreSetValue store i (key,lens);
                            return ();
                        };
                        Nothing -> return ();
                    };
                };
            } :: m ());

            vrFirstAspectGetter :: AspectGetter tedit;
            vrFirstAspectGetter = do
            {
                tsel <- treeViewGetSelection tview;
                ltpath <- treeSelectionGetSelectedRows tsel;
                case ltpath of
                {
                    [[i]] -> do
                    {
                        (key,_) <- listStoreGetValue store i;
                        return $ Just $ getaspect key;
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
