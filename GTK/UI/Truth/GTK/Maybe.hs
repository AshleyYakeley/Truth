{-# OPTIONS -fno-warn-orphans #-}
module UI.Truth.GTK.Maybe (maybeView,resultView) where
{
    import UI.Truth.GTK.GView;
    import Graphics.UI.Gtk;
    import Data.Changes;
    import Data.Witness;
    import Data.FunctorOne;
    import Data.Result;
    import Data.IORef;
    import Control.Applicative;

    pushButton :: Push edit -> edit -> String -> IO Button;
    pushButton push edit name = do
    {
        button <- buttonNew;
        set button [buttonLabel := name];
        onClicked button (do
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

    createButton :: (FullEdit edit) => Subject edit -> Push edit -> IO Button;
    createButton val push = pushButton push (replaceEdit val) "Create";

    functorOneIVF :: forall f edit wd.
    (
        HasNewValue1 f,
        Applicative f,
        FunctorOne f,
        HasNewValue (Subject edit),
        FullEdit edit,
        WidgetClass wd
    ) =>
      EditRepKTT f -> Maybe (forall b. f b) -> (Push (JustRepEdit f edit) -> IO wd) -> GView edit -> GView (JustRepEdit f edit);
    functorOneIVF repf mDeleteValue makeEmptywidget factory initial push = let
    {
        mpush :: Push edit;
        mpush ea = push (Right (MkJustEdit ea));
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
                doIf mDeleteButton (boxAddShow PackNatural box);
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
                        return (do
                        {
                            MkSelection _ (lens :: FloatingLens state edit editb) <- msel;
                            return
                             (
                                (newValue1 (Type :: Type (f (Subject editb))) MkSelection)
                                (justWholeEditRep repf)                             -- (TEditRepT (KTTEditRepKTT typeRepKKTTKTT repf) rep)    -- JustEdit repf rep
                                (resultLens lens)
                             );
                        });
                    };
                    Nothing -> return Nothing;
                };
            }),
            vrUpdate = \edit -> do
            {
                miv :: Maybe (GViewResult edit) <- readIORef stateRef;
                case miv of
                {
                    Just (MkViewResult ws update) -> case extractJustEdit edit of
                    {
                        Just edita -> update edita;
                        Nothing -> do
                        {
                            boxAddShow PackGrow box emptyWidget;
                            containerRemoveDestroy box (vwsWidget ws);
                            doIf mDeleteButton (containerRemove box);
                            writeIORef stateRef Nothing;
                        };
                    };
                    Nothing -> case edit of
                    {
                        Left (MkWholeEdit fa) | SuccessResult a <- retrieveOne fa -> do
                        {
                            iv@(MkViewResult ws _) <- factory a mpush;
                            doIf mDeleteButton (boxAddShow PackNatural box);
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

    maybeView :: (HasNewValue (Subject edit),FullEdit edit) =>
      GView edit -> GView (JustRepEdit Maybe edit);
    maybeView = functorOneIVF typeRepKTT (Just Nothing) (createButton (Just newValue));

    placeholderLabel :: IO Label;
    placeholderLabel = do
    {
        label <- labelNew (Just "Placeholder");
        return label;
    };

    resultView :: (HasNewValue (Subject edit),FullEdit edit) => EditRepT err -> GView edit -> GView (JustRepEdit (Result err) edit);
    resultView reperr = functorOneIVF (TEditRepKTT typeRepKTKTT reperr) Nothing (\_ -> placeholderLabel);
}
