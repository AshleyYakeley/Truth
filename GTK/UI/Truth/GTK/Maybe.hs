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

    createButton :: (CompleteEditScheme a edit) => a -> Push edit -> IO Button;
    createButton val push = pushButton push (replaceEdit val) "Create";

    functorOneIVF :: forall f a edit wd. 
    (
        HasTypeRep1 f,
        HasNewValue1 f,
        Applicative f,
        FunctorOne f,
        CompleteEditScheme a edit,
        WidgetClass wd
    ) =>
      Maybe (forall b. f b) -> (Push (JustEdit (f a) edit) -> IO wd) -> GView a edit -> GView (f a) (JustEdit (f a) edit);
    functorOneIVF mDeleteValue makeEmptywidget factory initial push = let
    {
        mpush :: Push edit;
        mpush ea = push (JustEdit ea);
    } in do
    {
        box <- vBoxNew True 0;
        emptyWidget <- makeEmptywidget push;
        mDeleteButton <- doIf mDeleteValue (\deleteValue -> pushButton push (replaceEdit (deleteValue :: f a)) "Delete");
        initialmiv :: Maybe (GViewResult a edit) <- case retrieveOne initial of
        {
            SuccessResult a -> do
            {
                iv@(MkViewResult ws _) <- factory a mpush;
                doIf mDeleteButton (containerAddShow box);
                containerAddShow box (wsWidget ws);
                return (Just iv);
            };
            _ -> do
            {
                containerAddShow box emptyWidget;
                return Nothing;
            };
        };
        stateRef :: IORef (Maybe (GViewResult a edit)) <- newIORef initialmiv;
        return (MkViewResult
        {
            vrWidget = MkWidgetStuff (toWidget box) (do
            {
                miv :: Maybe (GViewResult a edit) <- readIORef stateRef;
                case miv of
                {
                    Just (MkViewResult ws _) -> do
                    {
                        msel <- wsGetSelection ws;
                        return (do
                        {
                            MkSelection (lens :: FloatingLens state a edit b editb) state <- msel;
                            return ((newValue1 (Type :: Type (f b)) MkSelection) (resultLens lens) state);
                        });
                    };
                    Nothing -> return Nothing;
                };
            }),
            vrUpdate = \edit -> do
            {
                miv :: Maybe (GViewResult a edit) <- readIORef stateRef;
                case miv of
                {
                    Just (MkViewResult ws update) -> case extractJustEdit edit of
                    {
                        Just edita -> update edita;
                        Nothing -> do
                        {
                            containerAddShow box emptyWidget;
                            containerRemoveDestroy box (wsWidget ws);
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
                            containerAddShow box (wsWidget ws);
                            containerRemove box emptyWidget;
                            writeIORef stateRef (Just iv);
                        };
                        _ -> return ();
                    };
                };
            }
        });
    };

    maybeView :: (CompleteEditScheme a edit) =>
      a -> GView a edit -> GView (Maybe a) (JustEdit (Maybe a) edit);
    maybeView initialVal = functorOneIVF (Just Nothing) (createButton (Just initialVal));
    
    placeholderLabel :: IO Label;
    placeholderLabel = do
    {
        label <- labelNew (Just "Placeholder");
        return label;
    };
    
    resultView :: (CompleteEditScheme a edit,HasTypeRep err) => GView a edit -> GView (Result err a) (JustEdit (Result err a) edit);
    resultView = functorOneIVF Nothing (\_ -> placeholderLabel);
}
