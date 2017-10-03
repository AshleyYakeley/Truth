module Truth.UI.GTK.Labelled(labelledUIView) where
{
    import Shapes;
    import Graphics.UI.Gtk;
    import Truth.Core;
    import Truth.UI.GTK.GView;


    labelledUIView :: GetGView;
    labelledUIView = MkGetView $ \getview uispec -> do
    {
        MkUILabelled text spec <- isUISpec uispec;
        return $ mapIOView (labelWidget text) $ getview spec;
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
