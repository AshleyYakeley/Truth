{-# OPTIONS -fno-warn-orphans #-}
module Truth.UI.GTK.Maybe (maybeMatchView,resultMatchView) where
{
    import Truth.UI.GTK.GView;
    import Graphics.UI.Gtk;
    import Truth.Object;
    import Truth.Edit;
    import Data.Reity;
    import Data.HasNewValue;
    import Data.Witness;
    import Data.FunctorOne;
    import Data.Result;
    import Data.IORef;
    import Control.Applicative;


    type Push edit = edit -> IO ();

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

    mapSelection :: forall f edit. (FunctorOne f, Edit edit) =>
     Info f -> Aspect edit -> Aspect (JustWholeEdit f edit);
    mapSelection tf (MkAspect teditb tsubj (lens :: FloatingEditLens state edit editb)) = MkAspect
     (construct (MkMatchJustWholeEdit tf teditb tsubj))
     (applyInfo tf tsubj)
     (justWholeFloatingEditLens lens);

    functorOneIVF :: forall f edit wd.
    (
        Applicative f,
        FunctorOne f,
        HasNewValue (EditSubject edit),
        FullEdit edit,
        WidgetClass wd
    ) =>
      Info f -> Maybe (forall b. f b) -> (Push (JustWholeEdit f edit) -> IO wd) -> GView edit -> GView (JustWholeEdit f edit);
    functorOneIVF tf mDeleteValue makeEmptywidget factory initial push = let
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
                    Just (MkViewResult ws update) -> case extractJustWholeEdit edit of
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
      w edit -> GView edit -> GView (JustWholeEdit Maybe edit);
    maybeView _ = functorOneIVF info (Just Nothing) (createButton (Just newValue));

    placeholderLabel :: IO Label;
    placeholderLabel = do
    {
        label <- labelNew (Just "Placeholder");
        return label;
    };

    resultView :: (HasNewValue (EditSubject edit),FullEdit edit) => w edit -> Info err -> GView edit -> GView (JustWholeEdit (Result err) edit);
    resultView _ terr = functorOneIVF (applyInfo (info :: Info Result) terr) Nothing (\_ -> placeholderLabel);

    maybeMatchView :: GetView -> MatchView;
    maybeMatchView getView tedit = do
    {
        MkMatchJustWholeEdit tf te _ta <- matchProp $(type1[t|MatchJustWholeEdit|]) tedit;
        Refl <- testEquality tf (info :: Info (Type_KTT Maybe));
        Edit_Inst tsubj <- matchProp $(type1[t|Edit_Inst|]) te;
        FullEdit_Inst <- matchProp $(type1[t|FullEdit_Inst|]) te;
        HasNewValue_Inst <- matchProp $(type1[t|HasNewValue_Inst|]) tsubj;
        return (maybeView te (getView te));
    };

    resultMatchView :: GetView -> MatchView;
    resultMatchView getView tedit = do
    {
        MkMatchJustWholeEdit tf te _ta <- matchProp $(type1[t|MatchJustWholeEdit|]) tedit;
        MkMatch tr terr <- matchProp $(type1[t|Match|]) tf;
        Kind_T <- matchProp $(type1[t|Kind_T|]) terr;
        Refl <- testEquality tr (info :: Info (Type_KTKTT Result));
        Edit_Inst tsubj <- matchProp $(type1[t|Edit_Inst|]) te;
        FullEdit_Inst <- matchProp $(type1[t|FullEdit_Inst|]) te;
        HasNewValue_Inst <- matchProp $(type1[t|HasNewValue_Inst|]) tsubj;
        return (resultView te terr (getView te));
    };
}
