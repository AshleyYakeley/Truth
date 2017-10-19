module Truth.UI.GTK.Drag(dragGetView) where
{
    import Shapes;
    --import Graphics.UI.Gtk;
    import Truth.Core;
    import Truth.UI.GTK.GView;


    dragSourceGetView :: GetGView;
    dragSourceGetView = MkGetView $ \getview uispec -> do
    {
        MkUIDragSource _typename _lens spec <- isUISpec uispec;
        return $ getview spec;
    };

    dragDestinationGetView :: GetGView;
    dragDestinationGetView = MkGetView $ \getview uispec -> do
    {
        MkUIDragDestination _typename _lens spec <- isUISpec uispec;
        return $ getview spec;
    };

    dragGetView :: GetGView;
    dragGetView = dragSourceGetView <> dragDestinationGetView;
}
