module Truth.UI.GTK.CSS(cssGetView) where
{
    import Shapes;
    import Graphics.UI.Gtk as Gtk;
    import Graphics.UI.Gtk.General.StyleContext as Gtk;
    import Graphics.UI.Gtk.General.CssProvider as Gtk;
    import Truth.Core;
    import Truth.UI.GTK.Useful;
    import Truth.UI.GTK.GView;

    cssGetView :: GetGView;
    cssGetView = mconcat
    [
        MkGetView $ \getview uispec -> do
        {
            MkUIName name spec <- isUISpec uispec;
            return $ do
            {
                w <- getview spec;
                liftIO $ widgetSetName w name;
                return w;
            };
        },
        MkGetView $ \getview uispec -> do
        {
            MkUICSSClass cssclass spec <- isUISpec uispec;
            return $ do
            {
                w <- getview spec;
                liftIO $ do
                {
                    sc <- widgetGetStyleContext w;
                    styleContextAddClass sc cssclass;
                    return w;
                };
            };
        },
        MkGetView $ \getview uispec -> do
        {
            MkUICSSStyleSheet full priority css spec <- isUISpec uispec;
            return $ do
            {
                w <- getview spec;
                liftIO $ do
                {
                    provider <- cssProviderNew;
                    cssProviderLoadFromString provider css;
                    children <- widgetGetTree full w;
                    for_ children $ \child -> do
                    {
                        sc <- widgetGetStyleContext child;
                        styleContextAddProvider sc provider priority;
                    };
                    return w;
                };
            };
        }
    ];
}
