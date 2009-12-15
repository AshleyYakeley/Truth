{-# LANGUAGE ViewPatterns, FlexibleContexts #-}
module UI.Truth.GTK.Window where
{
    import UI.Truth.GTK.Text;
    import UI.Truth.GTK.CheckButton;
    import UI.Truth.GTK.Maybe;
    import UI.Truth.GTK.GView;
    import Graphics.UI.Gtk;
    import Data.Changes;
    import Data.IORef;

    makeButton :: String -> IO () -> IO Button;
    makeButton name action = do
    {
        button <- buttonNew;
        set button [buttonLabel := name];
        onClicked button action;
        return button;
    };

    lastResortView :: GetView;
    lastResortView _ = \_ _ -> do
    {
        w <- labelNew (Just "Uneditable");
        return (MkViewResult (MkViewWidgetStuff (toWidget w) (return Nothing)) (\_ -> return ()));
    };

    matchViews :: [MatchView];
    matchViews = [checkButtonMatchView,textMatchView,maybeMatchView getView,resultMatchView getView];

    getView :: GetView;
    getView = theView matchViews where
    {
        theView :: [MatchView] -> GetView;
        theView [] tedit = lastResortView tedit;
        theView (mview:mviews) tedit = case mview tedit of
        {
            Just gview -> gview;
            _ -> theView mviews tedit;
        };
    };

    makeWindow :: (Edit edit) => InfoT edit -> IORef Int -> IO () -> Subscribe edit -> IO ();
    makeWindow te ref tellclose sub = do
    {
        (sub',w,close) <- subscribeView (getView te) sub;
        window <- windowNew;
        box <- vBoxNew False 0;

        selectionButton <- makeButton "Selection" (do
        {
            msel <- vwsGetSelection w;
            case msel of
            {
                Just (MkAspect tsel _ta lens) -> do
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
