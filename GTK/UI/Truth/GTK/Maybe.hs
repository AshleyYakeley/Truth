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

    containerAddShow :: (ContainerClass w1, WidgetClass w2) => w1 -> w2 -> IO ();
    containerAddShow w1 w2 = do
    {
        containerAdd w1 w2;
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

    createButton :: (Edit edit) => Subject edit -> Push edit -> IO Button;
    createButton val push = pushButton push (replaceEdit val) "Create";

    functorOneIVF :: forall f edit wd. 
    (
        --HasTypeRepKTT f,
        HasNewValue1 f,
        Applicative f,
        FunctorOne f,
        HasNewValue (Subject edit),
        Edit edit,
        WidgetClass wd
    ) =>
      EditRepKTT f -> Maybe (forall b. f b) -> (Push (JustEdit f edit) -> IO wd) -> GView edit -> GView (JustEdit f edit);
    functorOneIVF repf mDeleteValue makeEmptywidget factory initial push = let
    {
        mpush :: Push edit;
        mpush ea = push (JustEdit ea);
    } in do
    {
        box <- vBoxNew True 0;
        emptyWidget <- makeEmptywidget push;
        mDeleteButton <- doIf mDeleteValue (\deleteValue -> pushButton push (replaceEdit (deleteValue :: f a)) "Delete");
        initialmiv :: Maybe (GViewResult edit) <- case retrieveOne initial of
        {
            SuccessResult a -> do
            {
                iv@(MkViewResult ws _) <- factory a mpush;
                doIf mDeleteButton (containerAddShow box);
                containerAddShow box (vwsWidget ws);
                return (Just iv);
            };
            _ -> do
            {
                containerAddShow box emptyWidget;
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
                            MkSelection rep (lens :: FloatingLens state edit editb) state <- msel;
                            return ((newValue1 (Type :: Type (f (Subject editb))) MkSelection) (TEditRepT (KTTEditRepKTT typeRepKKTTKTT repf) rep) (resultLens lens) state);
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
                            containerAddShow box emptyWidget;
                            containerRemoveDestroy box (vwsWidget ws);
                            doIf mDeleteButton (containerRemove box);
                            writeIORef stateRef Nothing;
                        };
                    };
                    Nothing -> case edit of
                    {
                        ReplaceJustEdit fa | SuccessResult a <- retrieveOne fa -> do
                        {
                            iv@(MkViewResult ws _) <- factory a mpush;                
                            doIf mDeleteButton (containerAddShow box);
                            containerAddShow box (vwsWidget ws);
                            containerRemove box emptyWidget;
                            writeIORef stateRef (Just iv);
                        };
                        _ -> return ();
                    };
                };
            }
        });
    };

    maybeView :: (HasNewValue (Subject edit),Edit edit) =>
      Subject edit -> GView edit -> GView (JustEdit Maybe edit);
    maybeView initialVal = functorOneIVF typeRepKTT (Just Nothing) (createButton (Just initialVal));
    
    placeholderLabel :: IO Label;
    placeholderLabel = do
    {
        label <- labelNew (Just "Placeholder");
        return label;
    };
    
    resultView :: (HasNewValue (Subject edit),Edit edit) => EditRepT err -> GView edit -> GView (JustEdit (Result err) edit);
    resultView reperr = functorOneIVF (TEditRepKTT typeRepKTKTT reperr) Nothing (\_ -> placeholderLabel);
}
