module Truth.UI.GTK.Maybe (oneUIView) where
{
    import Shapes;
    import Truth.Core;
    import Graphics.UI.Gtk hiding (get,Object);
    import Truth.UI.GTK.GView;


    boxAddShow :: (BoxClass w1, WidgetClass w2) => Packing -> w1 -> w2 -> IO ();
    boxAddShow packing w1 w2 = do
    {
        boxPackStart w1 w2 packing 0;
        widgetShow w2;
    };

    containerRemoveDestroy :: (ContainerClass w1, WidgetClass w2) => w1 -> w2 -> IO ();
    containerRemoveDestroy w1 w2 = do
    {
        containerRemove w1 w2;
        widgetDestroy w2;
    };

    createButton :: (FullEdit edit) => EditSubject edit -> Object edit -> IO Button;
    createButton subj object = makeButton "Create" $ runObject object $ \muted -> do
    {
        edits <- fromReadable (writerToReadable replaceEdit) subj;
        maction <- mutableEdit muted edits;
        case maction of
        {
            Just action -> action;
            Nothing -> return ();
        };
    };

    oneWholeView :: forall f edit wd.
    (
        MonadOne f,
        FullEdit edit,
        WidgetClass wd
    ) =>
      (forall editb. (FullEdit editb) => UISpec editb -> UISpec (OneWholeEdit f editb)) -> Maybe (Limit f) -> (Object (OneWholeEdit f edit) -> IO wd) -> GView edit -> GView (OneWholeEdit f edit);
    oneWholeView uispec mDeleteValue makeEmptywidget (MkView baseView) = MkView $ \object setSelect -> do
    {
        box <- vBoxNew False 0;
        emptyWidget <- makeEmptywidget object;
        mDeleteButton <- for mDeleteValue $ \(MkLimit deleteValue) -> makeButton "Delete" $ runObject object $ \muted -> do
        {
            maction <- mutableEdit muted [SumEditLeft $ MkWholeEdit deleteValue];
            case maction of
            {
                Just action -> action;
                Nothing -> return ();
            };
        };

        let
        {
            baseReadFunction :: ReadFunction (OneReader f (EditReader edit)) (EditReader edit);
            baseReadFunction rt = do
            {
                ft <- readable $ ReadOne rt;
                case retrieveOne ft of
                {
                    SuccessResult t -> return t;
                    FailureResult _ -> liftIO $ fail "read of nonexistant object";
                };
            };

            baseMuted :: MonadIO m => MutableEdit m (OneWholeEdit f edit) -> MutableEdit m edit;
            baseMuted (MkMutableEdit mr me) = MkMutableEdit (mapMutableRead baseReadFunction mr) $ \edits -> me $ fmap (SumEditRight . MkOneEdit) edits;

            baseObj = MkObject $ \call -> runObject object $ \muted -> call $ baseMuted muted;

            baseSetSelect :: AspectGetter edit -> IO ();
            baseSetSelect ag = setSelect $ fmap (fmap (mapOneWholeEditAspect uispec)) ag;

            getVR :: forall m. IsStateIO m => f () -> m (f (GViewResult edit));
            getVR fu = for fu $ \() -> liftIO $ baseView baseObj baseSetSelect;

            newWidgets :: f (GViewResult edit) -> IO ();
            newWidgets fg = case retrieveOne fg of
            {
                FailureResult (MkLimit _) -> do
                {
                    boxAddShow PackGrow box emptyWidget;
                };
                SuccessResult (MkViewResult w _ _) -> do
                {
                    for_ mDeleteButton (boxAddShow PackNatural box);
                    boxAddShow PackGrow box w;
                };
            };
        };

        firstfu <- runObject object $ \muted -> mutableRead muted ReadHasOne;
        firstfvr <- getVR firstfu;
        newWidgets firstfvr;
        stateVar :: MVar (f (GViewResult edit)) <- newMVar firstfvr;

        let
        {
            vrWidget = toWidget box;
            vrUpdate :: forall m. IsStateIO m => MutableRead m (OneReader f (EditReader edit)) -> [OneWholeEdit f edit] -> m ();
            vrUpdate mr wedits = mvarStateAccess stateVar $ do
            {
                oldfvr <- get;
                newfu <- lift $ mr ReadHasOne;
                newfvr <- case (retrieveOne oldfvr,retrieveOne newfu) of
                {
                    (SuccessResult (MkViewResult _ update _),SuccessResult ()) -> do
                    {
                        lift $ do
                        {
                            editss <- for wedits $ extractOneWholeEdit;
                            update (mapMutableRead baseReadFunction mr) $ mconcat editss;
                        };
                        return oldfvr;
                    };
                    (SuccessResult (MkViewResult w _ _),FailureResult (MkLimit newlf)) -> liftIO $ do
                    {
                        containerRemoveDestroy box w;
                        newWidgets newlf;
                        return newlf;
                    };
                    (FailureResult _,FailureResult (MkLimit newlf)) -> return newlf;
                    (FailureResult _,SuccessResult ()) -> do
                    {
                        newfvr <- getVR newfu;
                        for_ newfvr $ \_ -> liftIO $ containerRemove box emptyWidget;
                        liftIO $ newWidgets newfvr;
                        return newfvr;
                    };
                };
                put newfvr;
            };
            vrFirstAspectGetter = mvarStateAccess stateVar $ do
            {
                fvr <- get;
                case getMaybeOne fvr of
                {
                    Just (MkViewResult _ _ ag) -> liftIO $ fmap (fmap (mapOneWholeEditAspect uispec)) ag;
                    Nothing -> return Nothing;
                }
            };
        };
        runObject object $ \muted -> vrUpdate (mutableRead muted) [];
        return MkViewResult{..};
    };

    placeholderLabel :: IO Label;
    placeholderLabel = do
    {
        label <- labelNew (Just "Placeholder");
        return label;
    };

    oneUIView :: GetUIView;
    oneUIView = MkGetUIView $ \getview uispec -> do
    {
        uit <- isUISpec uispec;
        return $ case uit of
        {
            MkUIMaybe mnewval itemspec -> oneWholeView (MkUISpec . MkUIMaybe Nothing) (Just $ MkLimit Nothing) (createButton mnewval) $ getview itemspec;
            MkUIOneWhole itemspec -> oneWholeView (MkUISpec . MkUIOneWhole) Nothing (\_ -> placeholderLabel) $ getview itemspec;
        };
    };
}
