module Truth.UI.GTK.KeyContainer where
{
    import Data.Proxy;
    import Data.Type.Heterogeneous;
    import Data.Foldable;
    import Data.Containers (ContainerKey);
    import Data.MonoTraversable(Element);
    import Graphics.UI.Gtk;
    import Control.Monad.IsStateIO;
    import Data.KeyContainer;
    import Data.Reity;
    import Truth.Core;
    import Truth.UI.GTK.GView;


    keyContainerView :: forall cont edit. (Show (ContainerKey cont),IONewItemKeyContainer cont,HasKeyReader cont (EditReader edit),IOFullReader (EditReader edit),Edit edit) =>
        Info (EditReader edit) -> Info edit -> Info (Element cont) -> GView (KeyEdit cont edit);
    keyContainerView infoR infoE infoElement = MkView $ \(MkObject object) setSelect -> do
    {
        initialKeys <- object $ \muted -> mutableRead muted KeyReadKeys;
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

        _ <- onFocus box $ \_ -> do
        {
            setSelect ();
            return True;
        };

        let
        {
            vrWidget = toWidget box;

            findInStore :: ContainerKey cont -> IO (Maybe Int);
            findInStore key = do
            {
                keys <- listStoreToList store;
                return $ Prelude.lookup key $ zip keys [0..];
            };

            vrUpdate :: forall m. IsStateIO m => MutableRead m (KeyReader cont (EditReader edit)) -> [KeyEdit cont edit] -> m ();
            vrUpdate _ edits = liftIO $ for_ edits $ \edit -> case edit of
            {
                KeyDeleteItem key -> do
                {
                    mindex <- findInStore key;
                    case mindex of
                    {
                        Just index -> listStoreRemove store index;
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
            vrFirstSelState = Just ();
            vrGetSelection () = do
            {
                tsel <- treeViewGetSelection tview;
                ltpath <- treeSelectionGetSelectedRows tsel;
                case ltpath of
                {
                    [[i]] -> do
                    {
                        key <- listStoreGetValue store i;
                        return $ Just $ MkAspect (applyInfo (applyInfo (info @SumEdit) (applyInfo (info @WholeReaderEdit) (applyInfo (applyInfo (info @OneReader) (info @Maybe)) infoR))) (applyInfo (applyInfo (info @OneEdit) (info @Maybe)) infoE)) (applyInfo (info @Maybe) infoElement) $ toGeneralLens $ keyLens key;
                    };
                    _ -> return Nothing;
                };
            };
        };
        return MkViewResult{..};
    };

    keyContainerMatchView :: MatchView;
    keyContainerMatchView = namedMatchView "key container" $ \iedit -> do
    {
        MkSplitInfo ikc ie <- matchInfo iedit;
        MkSplitInfo ik ic <- matchInfo ikc;
        ReflH <- sameInfo (info :: Info KeyEdit) ik;
        ConstraintFact <- ask (infoKnowledge iedit) $ applyInfo (info @IONewItemKeyContainer) ic;
        ValueFact (MkContainerKeyInfo ikey) <- ask (infoKnowledge iedit) $ applyInfo (info @ContainerKeyInfo) ic;
        ConstraintFact <- ask (infoKnowledge iedit) $ applyInfo (info @Show) ikey;
        ValueFact (MkEditReaderInfo ir) <- ask (infoKnowledge iedit) $ applyInfo (info @EditReaderInfo) ie;
        ConstraintFact <- ask (infoKnowledge iedit) $ applyInfo (applyInfo (info @HasKeyReader) ic) ir;
        ValueFact (MkElementInfo ielem) <- ask (infoKnowledge iedit) $ applyInfo (info @ElementInfo) ic;
        ConstraintFact <- ask (infoKnowledge iedit) $ applyInfo (info @IOFullReader) ir;
        ConstraintFact <- ask (infoKnowledge iedit) $ applyInfo (info @Edit) ie;
        return $ keyContainerView ir ie ielem;
    };
}
