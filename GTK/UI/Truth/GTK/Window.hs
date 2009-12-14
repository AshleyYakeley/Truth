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

    lastResortView :: InfoT edit -> GView edit;
    lastResortView _ = \_ _ -> do
    {
        w <- labelNew (Just "Uneditable");
        return (MkViewResult (MkViewWidgetStuff (toWidget w) (return Nothing)) (\_ -> return ()));
    };

    type MatchView = forall edit. (Edit edit) => InfoT edit -> Maybe (GView edit);

    maybeMatchView :: MatchView;
    maybeMatchView tedit = do
    {
        MkMatchJustWholeEdit tf te _ta <- matchPropertyT_ (Type :: Type (MatchJustWholeEdit FT)) tedit;
        MkEqualType <- matchWitnessKTT tf (infoKTT :: InfoKTT Maybe);
        MkEditInst tsubj <- matchPropertyT te;
        MkFullEditInst <- matchPropertyT te;
        MkHasNewValueInst <- matchPropertyT tsubj;
        return (maybeView te (theView matchViews te));
    };

    resultMatchView :: MatchView;
    resultMatchView tedit = do
    {
        MkMatchJustWholeEdit tf te _ta <- matchPropertyT_ (Type :: Type (MatchJustWholeEdit FT)) tedit;
        MkTMatchKTT tr terr <- matchPropertyKTT_ (Type :: Type (TMatchKTT FKTT)) tf;
        MkEqualType <- matchWitnessKTKTT tr (infoKTKTT :: InfoKTKTT Result);
        MkEditInst tsubj <- matchPropertyT te;
        MkFullEditInst <- matchPropertyT te;
        MkHasNewValueInst <- matchPropertyT tsubj;
        return (resultView te terr (theView matchViews te));
    };

    checkButtonMatchView :: MatchView;
    checkButtonMatchView tedit = do
    {
        MkEqualType <- matchWitnessT tedit (infoT :: InfoT (WholeEdit Bool));
        return (checkButtonView "");
    };

    textMatchView :: MatchView;
    textMatchView tedit = do
    {
        MkEqualType <- matchWitness tedit (infoT :: InfoT (ListEdit (WholeEdit Char)));
        return textView;
    };

    matchViews :: [MatchView];
    matchViews = [checkButtonMatchView,textMatchView,maybeMatchView,resultMatchView];

    theView :: forall edit. (Edit edit) => [MatchView] -> InfoT edit -> GView edit;
    theView [] tedit = lastResortView tedit;
    theView (mview:mviews) tedit = case mview tedit of
    {
        Just gview -> gview;
        _ -> theView mviews tedit;
    };

    makeWindow :: (Edit edit) => InfoT edit -> IORef Int -> IO () -> Subscribe edit -> IO ();
    makeWindow te ref tellclose sub = do
    {
        (sub',w,close) <- subscribeView (theView matchViews te) sub;
        window <- windowNew;
        box <- vBoxNew False 0;

        selectionButton <- makeButton "Selection" (do
        {
            msel <- vwsGetSelection w;
            case msel of
            {
                Just (MkSelection tsel _ta lens) -> do
                {
                    makeWindowCountRef tsel ref (lensSubscribe lens sub');
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

    makeWindowCountRef :: (Edit edit) => InfoT edit -> IORef Int -> Subscribe edit -> IO ();
    makeWindowCountRef te windowCount sub = do
    {
        makeWindow te windowCount (do
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
