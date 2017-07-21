{-# OPTIONS -fno-warn-orphans #-}
module Truth.UI.GTK.Maybe (maybeTypeKnowledge) where
{
    --import Control.Applicative;
    --import Data.IORef;
    import Data.Result;
    import Data.MonadOne;
    --import Data.Witness;
    import Data.HasNewValue;
    --import Data.Type.Heterogeneous;
    import Data.Reity;
    import Truth.Core;
    import Graphics.UI.Gtk;
    import Truth.UI.GTK.GView;


    type Push edit = [edit] -> IO (Maybe (IO ()));
{-
    pushButton :: Push edit -> [edit] -> String -> IO Button;
    pushButton push edit name = do
    {
        button <- buttonNew;
        set button [buttonLabel := name];
        _ <- onClicked button (do
        {
            maction <- push edit;
            case maction of
            {
                Just action -> action;
                Nothing -> return ();
            };
        });
        return button;
    };

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
-}


{-
    doIf :: Maybe a -> (a -> IO b) -> IO (Maybe b);
    doIf (Just a) f = fmap Just (f a);
    doIf Nothing _ = return Nothing;
-}

    createButton :: (IOFullEdit edit) => EditSubject edit -> Push edit -> IO Button;
    createButton _ _ = undefined;
{-
    createButton object = runObject object $ do
    {
        edits <- unReadable (ioWriterToReadable ioReplaceEdit) (mutableRead muted) :: MutableRead m (EditReader edit) -> m [edit];
        maction <- mutableEdit muted edits;
        case maction of
        {
            Just action -> action;
            Nothing -> return ();
        };
    };
-}

{-
    newtype Object edit = MkObject {runObject :: forall r. (forall m. IsStateIO m => MutableEdit m edit -> m r) -> IO r};

    mapSelection :: forall f edit. (MonadOne f, Edit edit, FullReader (EditReader edit)) =>
     Info f -> Aspect edit -> Maybe (Aspect (OneWholeEdit f edit));
    mapSelection fi aspect = mapOneWholeEditAspect fi aspect;
-}
    monadOneIVF :: forall f edit wd.
    (
        Applicative f,
        MonadOne f,
        HasNewValue (EditSubject edit),
        FullEdit edit,
        WidgetClass wd
    ) =>
      Info f -> Maybe (Limit f) -> (Push (OneWholeEdit f edit) -> IO wd) -> GView edit -> GView (OneWholeEdit f edit);
    monadOneIVF _tf _mDeleteValue _makeEmptywidget _factory = MkView $ \_object _setSelect ->
    undefined;
{-
     let
    {
        mpush :: Push edit;
        mpush ea = push (Right (MkOneEdit ea));
    } in do
    {
        box <- vBoxNew False 0;
        emptyWidget <- makeEmptywidget push;
        mDeleteButton <- doIf mDeleteValue (\deleteValue -> pushButton push (replaceEdit (deleteValue :: f a)) "Delete");
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
    maybeView :: (HasNewValue (EditSubject edit),FullEdit edit) =>
      GView edit -> GView (OneWholeEdit Maybe edit);
    maybeView = monadOneIVF @Maybe info (Just $ MkLimit Nothing) (createButton (Just newValue));

    -- orphan
    instance (
        EditReader edit ~ reader,
        DependentHasView Widget edit,
        HasNewValue (ReaderSubject reader),
        FullEdit edit
        ) => DependentHasView Widget (SumWholeReaderEdit (OneReader Maybe reader) (OneEdit Maybe edit)) where
    {
        dependsView k iedit = do
        {
            MkSplitInfo _ iome <- matchInfo iedit;
            MkSplitInfo _ ie <- matchInfo iome;
            view <- dependsView k ie;
            return $ maybeView view;
        };
    };

    -- orphan
    instance (
        EditReader edit ~ reader,
        DependentHasView Widget edit,
        HasNewValue (ReaderSubject reader),
        FullEdit edit,
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

    resultView :: (HasNewValue (EditSubject edit),FullEdit edit) => Info err -> GView edit -> GView (OneWholeEdit (Result err) edit);
    resultView terr = monadOneIVF (applyInfo (info :: Info Result) terr) Nothing (\_ -> placeholderLabel);

    -- orphan
    instance
    (
        EditReader edit ~ reader,
        DependentHasView Widget edit,
        HasNewValue (ReaderSubject reader),
        FullEdit edit
    ) => DependentHasView Widget (SumWholeReaderEdit (OneReader (Result err) reader) (OneEdit (Result err) edit)) where
    {
        dependsView k iedit = do
        {
            MkSplitInfo _ iore <- matchInfo iedit;
            MkSplitInfo ior ie <- matchInfo iore;
            MkSplitInfo _ ir <- matchInfo ior;
            MkSplitInfo _ ierr <- matchInfo ir;
            view <- dependsView k ie;
            return $ resultView ierr view;
        };
    };

    -- orphan
    instance (
        EditReader edit ~ reader,
        DependentHasView Widget edit,
        HasNewValue (ReaderSubject reader),
        FullEdit edit,
        HasView Widget edit,
        HasInfo err
        ) => HasView Widget (SumWholeReaderEdit (OneReader (Result err) reader) (OneEdit (Result err) edit)) where
    {
        theView = resultView info theView;
    };

    maybeTypeKnowledge :: TypeKnowledge;
    maybeTypeKnowledge = mconcat
    [
        namedKnowledge "maybe" $(declInfo [d|
            instance
                (
                    EditReader edit ~ reader,
                    DependentHasView Widget edit,
                    HasNewValue (ReaderSubject reader),
                    FullEdit edit
                ) =>
                DependentHasView Widget (SumWholeReaderEdit (OneReader Maybe reader) (OneEdit Maybe edit));
            |]),
        namedKnowledge "result" $(declInfo [d|
            instance
                (
                    EditReader edit ~ reader,
                    DependentHasView Widget edit,
                    HasNewValue (ReaderSubject reader),
                    FullEdit edit
                ) =>
                DependentHasView Widget (SumWholeReaderEdit (OneReader (Result err) reader) (OneEdit (Result err) edit));
            |])
    ];

{-
    resultTypeKnowledge :: GetView -> TypeKnowledge;
    resultTypeKnowledge findView i = do
    {
        MkMatchOneWholeEdit fInfo eInfo rInfo <- matchInfo i;
        MkSplitInfo resInfo errInfo <- matchInfo fInfo;
        ReflH <- isInfo @Result resInfo;
        ValueFact (MkReaderSubjectInfo subjInfo) <- askInfo (infoKnowledge i) $ applyInfo (info @ReaderSubjectInfo) rInfo;
        ConstraintFact <- askInfo (infoKnowledge i) $ applyInfo (info @HasNewValue) subjInfo;
        ConstraintFact <- askInfo (infoKnowledge i) $ applyInfo (info @FullEdit) eInfo;
        return (resultView eInfo errInfo (findView eInfo));
    };
-}

}
