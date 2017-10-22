module Truth.UI.GTK.Labelled(labelledGetView) where
{
    import Shapes;
    import Graphics.UI.Gtk;
    import Truth.Core;
    import Truth.UI.GTK.GView;


    labelledGetView :: GetGView;
    labelledGetView = MkGetView $ \getview uispec -> do
    {
        MkUILabelled text spec <- isUISpec uispec;
        return $ do
        {
            view <- getview spec;
            liftIO $ labelWidget text view;
        };
    };

    labelWidget :: String -> Widget -> IO Widget;
    labelWidget text widget = do
    {
        box <- hBoxNew False 0;
        label <- labelNew $ Just $ text ++ ": ";
        boxPackStart box label PackNatural 0;
        boxPackStart box widget PackGrow 0;
        --for_ widgets $ \widget -> boxPackStart vbox widget (if any (isA widget) [gTypeViewport,gTypeTextView] then PackGrow else PackNatural) 0;
        return $ toWidget box;
    };
}
