{-# LANGUAGE ViewPatterns, FlexibleContexts #-}
module UI.Truth.GTK.Window where
{
    import UI.Truth.GTK.Text;
    import UI.Truth.GTK.CheckButton;
    import UI.Truth.GTK.Maybe;
    import UI.Truth.GTK.GView;
    import Graphics.UI.Gtk;
    import Data.Changes;
    import Data.Result;
    import Data.Witness;
    import Data.IORef;

    makeButton :: String -> IO () -> IO Button;
    makeButton name action = do
    {
        button <- buttonNew;
        set button [buttonLabel := name];
        onClicked button action;
        return button;
    };

    lastResortView :: GView edit;
    lastResortView = \_ _ -> do
    {
        w <- labelNew (Just "Uneditable");
        return (MkViewResult (MkViewWidgetStuff (toWidget w) (return Nothing)) (\_ -> return ()));
    };

    type MatchView = forall edit. (FullEdit edit,HasNewValue (Subject edit)) => Maybe (GView edit);

    matchJRE :: (HasTypeRepT edit) => Maybe (MatchJRE edit);
    matchJRE = Nothing;

    data MatchJRE edit where
    {
        MkMatchJRE :: (FullEdit edit,HasNewValue (Subject edit)) => TypeRepKTT f -> MatchJRE (JustRepEdit f edit);
    };

    maybeMatchView :: forall edit. (FullEdit edit,HasNewValue (Subject edit)) => Maybe (GView edit);
    maybeMatchView = do
    {
        MkMatchJRE repF <- matchJRE :: Maybe (MatchJRE edit);
        MkEqualType <- matchEditRepKTT repF (MkTypeRepKTT :: TypeRepKTT Maybe);
        return (maybeView (theView matchViews));
    };

    resultMatchView :: forall edit. (FullEdit edit,HasNewValue (Subject edit)) => Maybe (GView edit);
    resultMatchView = do
    {
        MkMatchJRE repF <- matchJRE :: Maybe (MatchJRE edit);
        return (resultView (theView matchViews));
    };
{-
    maybeMatchView :: MatchView;
    maybeMatchView repT = do
    {
        ff <- matchJustWholeEditRep repT;
        ff (foo repT);
    } where
    {
        foo :: forall t f edit. (Edit t) =>
          EditRepT t ->
          EqualType t (JustRepEdit f edit) -> EditRepKTT f -> EditRepT edit -> EditRepT (Subject edit) -> Maybe (GView t);
        foo repT MkEqualType repF repEdit repSubj = case editEvidence repT of
        {
            (_,MkEditInst) -> case editEvidence (Type :: Type (JustEdit f edit)) of
            {
                (MkHasNewValueInst,_,MkFullEditInst) -> do
                {
                    MkEqualType <- matchEditRepKTT repF (typeRepKTT :: EditRepKTT Maybe);
                    return (maybeView (theView matchViews repEdit))
                };
            };
        };
    };

    resultMatchView :: MatchView;
    resultMatchView repT = do
    {
        ff <- matchJustWholeEditRep repT;
        ff (foo repT);
    } where
    {
        foo :: forall t f edit. (Edit t) =>
          EditRepT t ->
          EqualType t (JustRepEdit f edit) -> EditRepKTT f -> EditRepT edit -> EditRepT (Subject edit) -> Maybe (GView t);
        foo repT MkEqualType repF repEdit repSubj = case editEvidence repT of
        {
            (_,MkEditInst) -> case editEvidence (Type :: Type (JustEdit f edit)) of
            {
                (MkHasNewValueInst,_,MkFullEditInst) -> case repF of
                {
                    (TEditRepKTT repR repErr) -> do
                    {
                        MkEqualType <- matchEditRepKTKTT repR (typeRepKTKTT :: EditRepKTKTT Result);
                        return (resultView repErr (theView matchViews repEdit))
                    };
                    _ -> Nothing;
                };
            };
        };
    };
-}
    checkButtonMatchView :: forall edit. (FullEdit edit,HasNewValue (Subject edit)) => Maybe (GView edit);
    checkButtonMatchView = do
    {
        MkEqualType <- matchWitness (typeRepT :: EditRepT edit) (typeRepT :: EditRepT (WholeEdit Bool));
        return (checkButtonView "");
    };

    textMatchView :: forall edit. (FullEdit edit,HasNewValue (Subject edit)) => Maybe (GView edit);
    textMatchView = do
    {
        MkEqualType <- matchWitness (typeRepT :: EditRepT edit) (typeRepT :: EditRepT (ListEdit (WholeEdit Char)));
        return textView;
    };

    matchViews :: [MatchView];
    matchViews = [checkButtonMatchView,textMatchView,maybeMatchView,resultMatchView];

    theView :: forall edit. (FullEdit edit,HasNewValue (Subject edit)) => [MatchView] -> GView edit;
    theView [] = lastResortView;
    theView (mview:mviews) = case mview of
    {
        Just gview -> gview;
        _ -> theView mviews;
    };

    makeWindow :: (FullEdit edit, HasNewValue (Subject edit)) => IORef Int -> IO () -> Subscribe edit -> IO ();
    makeWindow ref tellclose sub = do
    {
        (sub',w,close) <- subscribeView (theView matchViews) sub;
        window <- windowNew;
        box <- vBoxNew False 0;

        selectionButton <- makeButton "Selection" (do
        {
            msel <- vwsGetSelection w;
            case msel of
            {
                Just (MkSelection lens) -> do
                {
                    makeWindowCountRef ref (lensSubscribe lens sub');
                };
                _ -> return ();
            };
        });

        boxPackStart box selectionButton PackNatural 0;
        boxPackStart box (vwsWidget w) PackGrow 0;

        set window [containerChild := box];
        widgetShow (vwsWidget w);
        onDestroy window (do
        {
            close;
            tellclose;
        });
        widgetShowAll window;
        return ();
    };

    makeWindowCountRef :: (FullEdit edit, HasNewValue (Subject edit)) => IORef Int -> Subscribe edit -> IO ();
    makeWindowCountRef windowCount sub = do
    {
        makeWindow windowCount (do
        {
            i <- readIORef windowCount;
            writeIORef windowCount (i - 1);
            if i == 1
             then mainQuit
             else return ();
        }) sub;
        i <- readIORef windowCount;
        writeIORef windowCount (i + 1);
        return ();
    };
}
