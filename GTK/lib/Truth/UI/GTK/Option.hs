module Truth.UI.GTK.Option(optionGetView) where
{
    import Shapes;
    import Graphics.UI.Gtk as Gtk;
    import Truth.Core;
    import Truth.UI.GTK.Useful;
    import Truth.UI.GTK.GView;


    optionGetView :: GetGView;
    optionGetView = MkGetView $ \_ uispec -> do
    {
        MkUIOption itemsFunction whichLens <- isUISpec uispec;
        return $ optionView itemsFunction whichLens;
    };

    listStoreView :: (FullSubjectReader (EditReader edit),Edit edit) =>
        View (ListEdit [EditSubject edit] edit) (ListStore (EditSubject edit));
    listStoreView = do
    {
        initialList <- viewMutableRead $ unReadable subjectFromReader;
        store <- liftIO $ listStoreNew initialList;
        viewReceiveUpdate $ \_mr -> \case
        {
            ListEditItem (MkSequencePoint i) edit -> liftIO $ do
            {
                newval <- fromReadFunctionM (applyEdit edit) (listStoreGetValue store i);
                listStoreSetValue store i newval;
            };
            ListDeleteItem (MkSequencePoint i) -> liftIO $ listStoreRemove store i;
            ListInsertItem (MkSequencePoint i) item -> liftIO $ listStoreInsert store i item;
            ListClear -> liftIO $ listStoreClear store;
        };
        return store;
    };

    optionFromStore :: Eq t => ListStore (t,String) -> GView (WholeEdit t);
    optionFromStore store = do
    {
        widget <- liftIO $ do
        {
            widget <- comboBoxNewWithModel store;
            renderer <- cellRendererTextNew;
            cellLayoutPackStart widget renderer False;
            cellLayoutSetAttributes widget renderer store $ \(_,row) -> [cellText := row];
            return widget;
        };

        changedSignal <- viewOn widget changed $ viewMutableEdit $ \muted -> do
        {
            mi <- liftIO $ comboBoxGetActiveIter widget;
            maction <- case mi of
            {
                Just i -> do
                {
                    (t,_) <- liftIO $ listStoreGetValue store $ listStoreIterToIndex i;
                    mutableEdit muted [MkWholeEdit t];
                };
                Nothing -> return Nothing;
            };
            case maction of
            {
                Just action -> action;
                Nothing -> return ();
            };
        };

        viewReceiveUpdate $ \_mr (MkWholeEdit t) -> liftIO $ do
        {
            items <- listStoreToList store;
            case find (\(_,(t',_)) -> t == t') $ zip [(0 :: Int)..] items of
            {
                Just (i,_) -> do
                {
                    mti <- treeModelGetIter store [i];
                    case mti of
                    {
                        Just ti -> withSignalBlocked changedSignal $ comboBoxSetActiveIter widget ti;
                        Nothing -> return ();
                    };
                };
                Nothing -> return ();
            };
        };
        return $ toWidget widget;
    };

    optionView :: forall t tedit. (Eq t,Edit tedit) =>
        GeneralFunction tedit (ListEdit [(t,String)] (WholeEdit (t,String))) -> GeneralLens tedit (WholeEdit t) -> GView tedit;
    optionView itemsFunction whichLens = do
    {
        store <- mapView (readOnlyGeneralLens itemsFunction) listStoreView;
        mapView whichLens $ optionFromStore store;
    };
}
