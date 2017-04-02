{-# OPTIONS -fno-warn-orphans #-}
module Truth.UI.GTK.Maybe (maybeMatchView,resultMatchView) where
{
    import Control.Applicative;
    import Data.IORef;
    import Data.Result;
    import Data.MonadOne;
    import Data.Witness;
    import Data.HasNewValue;
    import Data.Type.Heterogeneous;
    import Data.Reity;
    import Truth.Edit;
    import Truth.Object;
    import Graphics.UI.Gtk;
    import Truth.UI.GTK.GView;


    type Push edit = edit -> IO (Maybe ());

    pushButton :: Push edit -> edit -> String -> IO Button;
    pushButton push edit name = do
    {
        button <- buttonNew;
        set button [buttonLabel := name];
        _ <- onClicked button (do
        {
            result <- push edit;
            case result of
            {
                Just _ -> return ();    -- succeeded
                _ -> return ();    -- failed. Could possibly do something here.
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

    doIf :: Maybe a -> (a -> IO b) -> IO (Maybe b);
    doIf (Just a) f = fmap Just (f a);
    doIf Nothing _ = return Nothing;

    createButton :: (FullEdit edit) => EditSubject edit -> Push edit -> IO Button;
    createButton val push = pushButton push (replaceEdit val) "Create";

    mapSelection :: forall f edit. (MonadOne f, Edit edit, FullReader (EditReader edit)) =>
     Info f -> Aspect edit -> Maybe (Aspect (OneWholeEdit f edit));
    mapSelection fi aspect = mapOneWholeEditAspect fi aspect;

    functorOneIVF :: forall f edit wd.
    (
        Applicative f,
        MonadOne f,
        HasNewValue (EditSubject edit),
        FullEdit edit,
        WidgetClass wd
    ) =>
      Info f -> Maybe (Limit f) -> (Push (OneWholeEdit f edit) -> IO wd) -> GView edit -> GView (OneWholeEdit f edit);
    functorOneIVF tf mDeleteValue makeEmptywidget factory initial push = let
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

    maybeView :: (HasNewValue (EditSubject edit),FullEdit edit) =>
      w edit -> GView edit -> GView (OneWholeEdit Maybe edit);
    maybeView _ = functorOneIVF @Maybe info (Just $ MkLimit Nothing) (createButton (Just newValue));

    placeholderLabel :: IO Label;
    placeholderLabel = do
    {
        label <- labelNew (Just "Placeholder");
        return label;
    };

    resultView :: (HasNewValue (EditSubject edit),FullEdit edit) => w edit -> Info err -> GView edit -> GView (OneWholeEdit (Result err) edit);
    resultView _ terr = functorOneIVF (applyInfo (info :: Info Result) terr) Nothing (\_ -> placeholderLabel);

    maybeMatchView :: GetView -> MatchView;
    maybeMatchView getView i = do
    {
        MkMatchOneWholeEdit fInfo eInfo rInfo <- matchInfo i;
        ReflH <- isInfo @Maybe fInfo;
        ValueFact (MkReaderSubjectInfo subjInfo) <- ask (infoKnowledge i) $ applyInfo (info @ReaderSubjectInfo) rInfo;
        ConstraintFact <- ask (infoKnowledge i) $ applyInfo (info @HasNewValue) subjInfo;
        ConstraintFact <- ask (infoKnowledge i) $ applyInfo (info @FullEdit) eInfo;
        return (maybeView eInfo (getView eInfo));
    };

    resultMatchView :: GetView -> MatchView;
    resultMatchView getView i = do
    {
        MkMatchOneWholeEdit fInfo eInfo rInfo <- matchInfo i;
        MkSplitInfo resInfo errInfo <- matchInfo fInfo;
        ReflH <- isInfo @Result resInfo;
        ValueFact (MkReaderSubjectInfo subjInfo) <- ask (infoKnowledge i) $ applyInfo (info @ReaderSubjectInfo) rInfo;
        ConstraintFact <- ask (infoKnowledge i) $ applyInfo (info @HasNewValue) subjInfo;
        ConstraintFact <- ask (infoKnowledge i) $ applyInfo (info @FullEdit) eInfo;
        return (resultView eInfo errInfo (getView eInfo));
    };
}
