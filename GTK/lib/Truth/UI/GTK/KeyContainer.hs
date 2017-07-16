module Truth.UI.GTK.KeyContainer where
{
    import Data.Proxy;
    import Data.Type.Heterogeneous;
    import Data.Foldable;
    import Data.Containers (ContainerKey);
    import Data.Empty;
    import Graphics.UI.Gtk;
    import Control.Monad.IsStateIO;
    import Data.KeyContainer;
    import Data.Reity;
    import Truth.Core;
    import Truth.UI.GTK.GView;


    keyContainerView :: forall cont edit. (KeyContainer cont) => GView (KeyEdit cont edit);
    keyContainerView = MkView $ \(MkObject object) _setSelect -> do
    {
        initialKeys <- object $ \muted -> mutableRead muted KeyReadKeys;
        store <- listStoreNew initialKeys;
        widget <- treeViewNewWithModel store;

        let
        {
            vrWidget = toWidget widget;

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
            vrFirstSelState = Nothing;
            vrGetSelection (ss :: None) = never ss;
        };
        return MkViewResult{..};
    };

    keyContainerMatchView :: MatchView;
    keyContainerMatchView = namedMatchView "key container" $ \iedit -> do
    {
        MkSplitInfo ikc _ie <- matchInfoNamed iedit;
        MkSplitInfo ik ic <- matchInfoNamed ikc;
        ReflH <- testHetEqualityNamed (info :: Info KeyEdit) ik;
        ConstraintFact <- askNamed (infoKnowledge iedit) $ applyInfo (info @KeyContainer) ic;
        return keyContainerView;
    };
}
