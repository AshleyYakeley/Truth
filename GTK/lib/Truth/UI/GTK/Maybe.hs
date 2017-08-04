{-# OPTIONS -fno-warn-orphans #-}
module Truth.UI.GTK.Maybe (maybeTypeKnowledge) where
{
    import Prelude;
    import Data.Foldable;
    import Data.Traversable;
    import Control.Concurrent.MVar;
    import Control.Monad.IO.Class;
    import Control.Monad.Trans.Class;
    import Control.Monad.Trans.State;
    import Data.Result;
    import Data.MonadOne;
    import Data.HasNewValue;
    import Control.Monad.IsStateIO;
    import Data.Reity;
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

    createButton :: (IOFullEdit edit) => EditSubject edit -> Object edit -> IO Button;
    createButton subj object = makeButton "Create" $ runObject object $ \muted -> do
    {
        edits <- fromReadable (writerToReadable ioReplaceEdit) subj;
        maction <- mutableEdit muted edits;
        case maction of
        {
            Just action -> action;
            Nothing -> return ();
        };
    };

    monadOneIVF :: forall f edit wd.
    (
        MonadOne f,
        IOFullEdit edit,
        WidgetClass wd
    ) =>
      TypeInfo f -> Maybe (Limit f) -> (Object (OneWholeEdit f edit) -> IO wd) -> GView edit -> GView (OneWholeEdit f edit);
    monadOneIVF tf mDeleteValue makeEmptywidget (MkView baseView) = MkView $ \object setSelect -> do
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
            baseReadFunction :: ReadFunction MonadIO (OneReader f (EditReader edit)) (EditReader edit);
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

            getVR :: forall m. IsStateIO m => MutableRead m (OneReader f (EditReader edit)) -> m (f (GViewResult edit));
            getVR mr = do
            {
                fu <- mr ReadHasOne;
                for fu $ \() -> do
                {
                    let
                    {
                        baseObj = MkObject $ \call -> runObject object $ \muted -> call $ baseMuted muted;
                        baseSetSelect ag = setSelect $ fmap (fmap (\maspect -> maspect >>= mapOneWholeEditAspect tf)) ag;
                    };
                    liftIO $ baseView baseObj baseSetSelect;
                };
            };

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

        fwma <- runObject object $ \muted -> getVR $ mutableRead muted;
        newWidgets fwma;
        stateVar :: MVar (f (GViewResult edit)) <- newMVar fwma;

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
                            editss <- for wedits $ extractOneWholeEdit @MonadIO;
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
                        fvr <- lift $ getVR mr;
                        for_ fvr $ \_ -> do
                        {
                            liftIO $ containerRemove box emptyWidget;
                        };
                        liftIO $ newWidgets fvr;
                        return fvr;
                    };
                };
                put newfvr;
            };
            vrFirstAspectGetter = mvarStateAccess stateVar $ do
            {
                fvr <- get;
                case getMaybeOne fvr of
                {
                    Just (MkViewResult _ _ ag) -> liftIO $ fmap (fmap (\maspect -> maspect >>= mapOneWholeEditAspect tf)) ag;
                    Nothing -> return Nothing;
                }
            };
        };
        runObject object $ \muted -> vrUpdate (mutableRead muted) [];
        return MkViewResult{..};
    };

{-
     let
    {
        mpush :: Push edit;
        mpush ea = push (Right (MkOneEdit ea));
    } in do
    {
        box <- vBoxNew False 0;
        emptyWidget <- makeEmptywidget push;
        mDeleteButton <- doIf mDeleteValue (\deleteValue -> pushButton push (pureReplaceEdit (deleteValue :: f a)) "Delete");
        initialmiv :: Maybe (GViewResult edit) <- case retrieveOne initial of
        {
            SuccessResult a -> do
            {
                iv@(MkViewResult ws _) <- factory a mpush;
                _ <- doIf mDeleteButton (boxAddShow PackNatural box);
                boxAddShow PackGrow box (vwsWidget ws);
                return (Just iv);
            };
            _ -> do
            {
                boxAddShow PackGrow box emptyWidget;
                return Nothing;
            };
        };
        stateRef :: IORef (Maybe (GViewResult edit)) <- newIORef initialmiv;
        return (MkViewResult
        {
            vrWidgetStuff = MkViewWidgetStuff (toWidget box) (do
            {
                miv :: Maybe (GViewResult edit) <- readIORef stateRef;
                case miv of
                {
                    Just (MkViewResult ws _) -> do
                    {
                        msel <- vwsGetSelection ws;
                        return (fmap (mapSelection tf) msel);
                    };
                    Nothing -> return Nothing;
                };
            }),
            vrUpdate = \edit -> do
            {
                miv :: Maybe (GViewResult edit) <- readIORef stateRef;
                case miv of
                {
                    Just (MkViewResult ws update) -> case extractOneWholeEdit edit of
                    {
                        Just edita -> update edita;
                        Nothing -> do
                        {
                            boxAddShow PackGrow box emptyWidget;
                            containerRemoveDestroy box (vwsWidget ws);
                            _ <- doIf mDeleteButton (containerRemove box);
                            writeIORef stateRef Nothing;
                        };
                    };
                    Nothing -> case edit of
                    {
                        Left (MkWholeEdit fa) | SuccessResult a <- retrieveOne fa -> do
                        {
                            iv@(MkViewResult ws _) <- factory a mpush;
                            _ <- doIf mDeleteButton (boxAddShow PackNatural box);
                            boxAddShow PackGrow box (vwsWidget ws);
                            containerRemove box emptyWidget;
                            writeIORef stateRef (Just iv);
                        };
                        _ -> return ();
                    };
                };
            }
        });
    };
-}
    maybeView :: (HasNewValue (EditSubject edit),IOFullEdit edit) =>
      GView edit -> GView (OneWholeEdit Maybe edit);
    maybeView = monadOneIVF @Maybe typeInfo (Just $ MkLimit Nothing) (createButton (Just newValue));

    -- orphan
    instance (
        EditReader edit ~ reader,
        DependentHasView Widget edit,
        HasNewValue (ReaderSubject reader),
        IOFullEdit edit
        ) => DependentHasView Widget (SumWholeReaderEdit (OneReader Maybe reader) (OneEdit Maybe edit)) where
    {
        dependsView = $(generateTypeMatchExpr [t|forall e r. SumWholeReaderEdit (OneReader Maybe r) (OneEdit Maybe e)|] [e|\ie _ -> do
        {
            view <- dependsView ie;
            return $ maybeView view;
        }|]);
    };

    -- orphan
    instance (
        EditReader edit ~ reader,
        DependentHasView Widget edit,
        HasNewValue (ReaderSubject reader),
        IOFullEdit edit,
        HasView Widget edit
        ) => HasView Widget (SumWholeReaderEdit (OneReader Maybe reader) (OneEdit Maybe edit)) where
    {
        theView = maybeView theView;
    };

    placeholderLabel :: IO Label;
    placeholderLabel = do
    {
        label <- labelNew (Just "Placeholder");
        return label;
    };

    resultView :: IOFullEdit edit => TypeInfo err -> GView edit -> GView (OneWholeEdit (Result err) edit);
    resultView terr = monadOneIVF (applyTypeInfo (typeInfo :: TypeInfo Result) terr) Nothing (\_ -> placeholderLabel);

    -- orphan
    instance
    (
        EditReader edit ~ reader,
        DependentHasView Widget edit,
        IOFullEdit edit
    ) => DependentHasView Widget (SumWholeReaderEdit (OneReader (Result err) reader) (OneEdit (Result err) edit)) where
    {
        dependsView = $(generateTypeMatchExpr [t|forall edit' reader' err'. SumWholeReaderEdit (OneReader (Result err') reader') (OneEdit (Result err') edit')|] [e|\ie _ ierr -> do
        {
            view <- dependsView ie;
            return $ resultView ierr view;
        }|]);
    };

    -- orphan
    instance (
        EditReader edit ~ reader,
        DependentHasView Widget edit,
        IOFullEdit edit,
        HasView Widget edit,
        HasTypeInfo err
        ) => HasView Widget (SumWholeReaderEdit (OneReader (Result err) reader) (OneEdit (Result err) edit)) where
    {
        theView = resultView typeInfo theView;
    };

    maybeTypeKnowledge :: TypeKnowledge;
    maybeTypeKnowledge = mconcat
    [
        namedKnowledge "maybe" $(generateTypeKnowledge [d|
            instance
                (
                    EditReader edit ~ reader,
                    DependentHasView Widget edit,
                    HasNewValue (ReaderSubject reader),
                    IOFullEdit edit
                ) =>
                DependentHasView Widget (SumWholeReaderEdit (OneReader Maybe reader) (OneEdit Maybe edit));
            |]),
        namedKnowledge "result" $(generateTypeKnowledge [d|
            instance
                (
                    EditReader edit ~ reader,
                    DependentHasView Widget edit,
                    HasNewValue (ReaderSubject reader),
                    IOFullEdit edit
                ) =>
                DependentHasView Widget (SumWholeReaderEdit (OneReader (Result err) reader) (OneEdit (Result err) edit));
            |])
    ];
}
