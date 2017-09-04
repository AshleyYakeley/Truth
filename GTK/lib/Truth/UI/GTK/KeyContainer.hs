module Truth.UI.GTK.KeyContainer(keyContainerUIView) where
{
    import Shapes;
    import Graphics.UI.Gtk;
    import Truth.Core;
    import Truth.UI.GTK.GView;


    pairAspect :: (KeyContainer cont,Show (ContainerKey cont),IOFullEdit valueedit,Edit keyedit,IOFullReader (EditReader keyedit),HasKeyReader cont (PairEditReader keyedit valueedit)) =>
        UISpec valueedit -> ContainerKey cont -> Maybe (Aspect (KeyEdit cont (PairEdit keyedit valueedit)));
    pairAspect uispec key = Just $ MkAspect (show key) (MkUISpec $ MkUIMaybe Nothing uispec) $ toGeneralLens $ keyValueLens key;

    keyContainerView :: forall cont edit. (Show (ContainerKey cont),IONewItemKeyContainer cont) =>
        (ContainerKey cont -> Maybe (Aspect (KeyEdit cont edit))) -> GView (KeyEdit cont edit);
    keyContainerView getAspect = MkView $ \(MkObject object) setSelect -> do
    {
        MkFiniteSet initialKeys <- object $ \muted -> mutableRead muted KeyReadKeys;
        store <- listStoreNew initialKeys;
        tview <- treeViewNewWithModel store;
        keyColRenderer <- cellRendererTextNew;
        namecolumn <- treeViewColumnNew;
        treeViewColumnSetTitle namecolumn "Key";
        cellLayoutPackStart namecolumn keyColRenderer False;
        cellLayoutSetAttributes namecolumn keyColRenderer store $ \key -> [cellText := show key];
        _ <- treeViewAppendColumn tview namecolumn;

        box <- vBoxNew False 0;
        newButton <- makeButton "New" $ object $ \muted -> do
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

            findInStore :: ContainerKey cont -> IO (Maybe Int);
            findInStore key = do
            {
                kk <- listStoreToList store;
                return $ lookup @[(ContainerKey cont,Int)] key $ zip kk [0..];
            };

            vrUpdate :: forall m. IsStateIO m => MutableRead m (KeyReader cont (EditReader edit)) -> [KeyEdit cont edit] -> m ();
            vrUpdate _ edits = liftIO $ for_ edits $ \edit -> case edit of
            {
                KeyDeleteItem key -> do
                {
                    mindex <- findInStore key;
                    case mindex of
                    {
                        Just i -> listStoreRemove store i;
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
                            _ <- listStoreAppend store key;
                            return ();
                        };
                    };
                };
                KeyClear -> listStoreClear store;
                _ -> return ();
            };

            vrFirstAspectGetter :: AspectGetter (KeyEdit cont edit);
            vrFirstAspectGetter = do
            {
                tsel <- treeViewGetSelection tview;
                ltpath <- treeSelectionGetSelectedRows tsel;
                case ltpath of
                {
                    [[i]] -> do
                    {
                        key <- listStoreGetValue store i;
                        return $ getAspect key;
                    };
                    _ -> return Nothing;
                };
            };
        };

        _ <- onFocus box $ \_ -> do
        {
            setSelect vrFirstAspectGetter;
            return True;
        };
        return MkViewResult{..};
    };

    keyContainerUIView :: GetUIView;
    keyContainerUIView = MkGetUIView $ \_ uispec -> fmap (\case
    {
        MkUIKeyContainer itemspec -> keyContainerView $ pairAspect itemspec;
    }) $ isUISpec uispec;
}
