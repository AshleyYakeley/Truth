module Truth.UI.GTK.Tuple(verticalLayoutGetView) where
{
    import Shapes;
    import Graphics.UI.Gtk;
    import Truth.Core;
    import Truth.UI.GTK.GView;


    verticalLayoutGetView :: GetGView;
    verticalLayoutGetView = MkGetView $ \getview uispec -> do
    {
        MkUIVertical aspects <- isUISpec uispec;
        return $ mapIOView arrangeWidgets $ for aspects getview;
    };

    arrangeWidgets :: [Widget] -> IO Widget;
    arrangeWidgets widgets = do
    {
        vbox <- vBoxNew False 0;
        for_ widgets $ \widget -> boxPackStart vbox widget (if any (isA widget) [gTypeViewport,gTypeTextView] then PackGrow else PackNatural) 0;
        return $ toWidget vbox;
    };
}
