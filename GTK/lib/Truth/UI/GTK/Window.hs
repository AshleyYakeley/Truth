{-# LANGUAGE ViewPatterns, FlexibleContexts #-}
module Truth.UI.GTK.Window where
{
    import Data.IORef;
    import Data.Reity;
    import Truth.Core;
    import Graphics.UI.Gtk;
    import Truth.UI.GTK.GView;
    --import Truth.UI.GTK.Maybe;
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
    lastResortView _ = MkView $ \_ _ -> do
    {
        w <- labelNew (Just "Uneditable");
        let
        {
            vrWidget = toWidget w;
            vrFirstUpdateState = ();
            vrUpdate _ _ _ = return ();
            vrFirstSelState = Nothing;
            vrGetSelection _ = return Nothing;
        };
        return MkViewResult{..};
    };

    matchViews :: [MatchView];
    matchViews = [checkButtonMatchView,textMatchView {-,maybeMatchView getView,resultMatchView getView-}];

    getView :: GetView;
    getView = finalGetView (mconcat matchViews) lastResortView;

    makeWindow :: (Edit edit) => Info edit -> IORef Int -> IO () -> Subscription edit -> IO ();
    makeWindow te ref tellclose sub = do
    {
        MkSubscriptionView{..} <- viewSubscription (getView te) sub;
        window <- windowNew;
        box <- vBoxNew False 0;

        selectionButton <- makeButton "Selection" (do
        {
            msel <- srGetSelection;
            case msel of
            {
                Just (MkAspect tsel _ta lens) -> do
                {
                    makeWindowCountRef tsel ref (mapSubscription lens sub);
                };
                _ -> return ();
            };
        });

        -- this is only correct if srWidget has native scroll support, such as TextView
        sw <- scrolledWindowNew Nothing Nothing;
        set sw [containerChild := srWidget];

        boxPackStart box selectionButton PackNatural 0;
        boxPackStart box sw PackGrow 0;

        set window [containerChild := box];
        widgetShow srWidget;
        _ <- onDestroy window (do
        {
            srClose;
            tellclose;
        });
        widgetShowAll window;
        return ();
    };

    makeWindowCountRef :: (Edit edit) => Info edit -> IORef Int -> Subscription edit -> IO ();
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
