{-# LANGUAGE ViewPatterns, FlexibleContexts #-}
module Truth.UI.GTK.Window where
{
    import Data.IORef;
    import Truth.TypeKT;
    import Truth.Core;
    import Graphics.UI.Gtk;
    import Truth.UI.GTK.GView;
    import Truth.UI.GTK.Maybe;
    import Truth.UI.GTK.CheckButton;
    import Truth.UI.GTK.Text;


    makeButton :: String -> IO () -> IO Button;
    makeButton name action = do
    {
        button <- buttonNew;
        set button [buttonLabel := name];
        _ <- onClicked button action;
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

    makeWindow :: (Edit edit) => Info (Type_T edit) -> IORef Int -> IO () -> Subscribe edit -> IO ();
    makeWindow te ref tellclose sub = do
    {
        (sub',w,close) <- viewSubscription (getView te) sub;
        window <- windowNew;
        box <- vBoxNew False 0;

        selectionButton <- makeButton "Selection" (do
        {
            msel <- vwsGetSelection w;
            case msel of
            {
                Just (MkAspect tsel _ta lens) -> do
                {
                    makeWindowCountRef tsel ref (mapSubscription lens sub');
                };
                _ -> return ();
            };
        });

        boxPackStart box selectionButton PackNatural 0;
        boxPackStart box (vwsWidget w) PackGrow 0;

        set window [containerChild := box];
        widgetShow (vwsWidget w);
        _ <- onDestroy window (do
        {
            close;
            tellclose;
        });
        widgetShowAll window;
        return ();
    };

    makeWindowCountRef :: (Edit edit) => Info (Type_T edit) -> IORef Int -> Subscribe edit -> IO ();
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
