module Truth.UI.GTK.KeyContainer(keyContainerUIView) where
{
    import Shapes;
    import Graphics.UI.Gtk;
    import Truth.Core;
    import Truth.UI.GTK.GView;


    addColumn :: TreeView -> ListStore (key,t) -> String -> (t -> String) -> IO ();
    addColumn tview store name showCell = do
    {
        renderer <- cellRendererTextNew;
        column <- treeViewColumnNew;
        treeViewColumnSetTitle column name;
        cellLayoutPackStart column renderer False;
        cellLayoutSetAttributes column renderer store $ \(_,t) -> [cellText := showCell t];
        _ <- treeViewAppendColumn tview column;
        return ();
    };

    data KeyColumns edit = forall tt. MkKeyColumns (ObjectFunction edit (WholeEdit tt)) [(String,tt -> String)];

    oneKeyColumn :: KeyColumn edit -> KeyColumns edit;
    oneKeyColumn (MkKeyColumn n f) = MkKeyColumns f [(n,id)];

    instance Semigroup (KeyColumns edita) where
    {
        MkKeyColumns f1 c1 <> MkKeyColumns f2 c2 = MkKeyColumns (pairWholeObjectFunction f1 f2) $ fmap (\(n,g) -> (n,g . fst)) c1 <> fmap (\(n,g) -> (n,g . snd)) c2;
    };

    instance Monoid (KeyColumns edita) where
    {
        mempty = MkKeyColumns unitWholeObjectFunction [];
        mappend = (<>);
    };

    keyContainerView :: forall cont cedit iedit. (IONewItemKeyContainer cont,FullSubjectReader (EditReader iedit),Edit cedit,Edit iedit,HasKeyReader cont (EditReader iedit)) =>
        KeyColumns (ContextEdit cedit iedit) -> Aspect (ContextEdit cedit (OneWholeEdit Maybe iedit)) -> GView (ContextEdit cedit (KeyEdit cont iedit));
    keyContainerView (MkKeyColumns (colfunc :: ObjectFunction (ContextEdit cedit iedit) (WholeEdit tt)) cols) aspect = MkView $ \(MkObject object :: Object (ContextEdit cedit (KeyEdit cont iedit))) setSelect -> do
    {
        let
        {
            getTT :: forall m. MonadIO m => ContainerKey cont -> MutableRead m (ContextEditReader cedit (KeyEdit cont iedit)) -> m tt;
            getTT key mr = unReadable (editGet colfunc () ReadWhole) $ mapMutableRead (liftContextReadFunction $ knownKeyItemReadFunction @cont key) mr;
        };
        initialTT <- object $ \muted -> do
        {
            MkFiniteSet initialKeys <- mutableRead muted $ MkTupleEditReader EditContent KeyReadKeys;
            for initialKeys $ \key -> do
            {
                tt <- getTT key $ mutableRead muted;
                return (key,tt);
            };
        };
        store <- listStoreNew initialTT;
        tview <- treeViewNewWithModel store;
        for_ cols $ \(name,showCell) -> addColumn tview store name showCell;

        box <- vBoxNew False 0;
        newButton <- makeButton "New" $ object $ \muted -> do
        {
            item <- liftIO $ newKeyContainerItem (Proxy::Proxy cont);
            maction <- mutableEdit muted [MkTupleEdit EditContent $ KeyInsertReplaceItem item];
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

            vrUpdate :: forall m. IsStateIO m => MutableRead m (ContextEditReader cedit (KeyEdit cont iedit)) -> [ContextEdit cedit (KeyEdit cont iedit)] -> m ();
            vrUpdate mr edits = for_ edits $ \edit -> (case edit of
            {
                MkTupleEdit EditContent (KeyDeleteItem key) -> do
                {
                    mindex <- findInStore key;
                    case mindex of
                    {
                        Just i -> liftIO $ listStoreRemove store i;
                        Nothing -> return ();
                    };
                };
                MkTupleEdit EditContent (KeyInsertReplaceItem item) -> let
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
                            tt <- getTT key mr;
                            _ <- liftIO $ listStoreAppend store (key,tt);
                            return ();
                        };
                    };
                };
                MkTupleEdit EditContent KeyClear -> liftIO $ listStoreClear store;
                MkTupleEdit EditContent (KeyEditItem key _) -> do
                {
                    mindex <- findInStore key;
                    case mindex of
                    {
                        Just i -> do
                        {
                            tt <- getTT key mr;
                            liftIO $ listStoreSetValue store i (key,tt);
                        };
                        Nothing -> return ();
                    };
                };
                MkTupleEdit EditContext _ -> return ();
            } :: m ());

            vrFirstAspectGetter :: AspectGetter (ContextEdit cedit (KeyEdit cont iedit));
            vrFirstAspectGetter = do
            {
                tsel <- treeViewGetSelection tview;
                ltpath <- treeSelectionGetSelectedRows tsel;
                case ltpath of
                {
                    [[i]] -> do
                    {
                        (key,_) <- listStoreGetValue store i;
                        return $ Just $ mapAspect (liftContextGeneralLens $ keyElementLens key) aspect;
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
        MkUIContextTable cols aspect <- isUISpec uispec;
        return $ keyContainerView (mconcat $ fmap oneKeyColumn cols) aspect;
    };
}
