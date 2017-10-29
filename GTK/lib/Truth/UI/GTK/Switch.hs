module Truth.UI.GTK.Switch (switchGetView) where
{
    import Shapes;
    import Truth.Core;
    import Graphics.UI.Gtk hiding (get);
    import Truth.UI.GTK.GView;


    boxAddShow :: (BoxClass w1, WidgetClass w2) => Packing -> w1 -> w2 -> IO ();
    boxAddShow packing w1 w2 = do
    {
        boxPackStart w1 w2 packing 0;
        widgetShow w2;
    };

    containerRemoveDestroy :: (ContainerClass w1, WidgetClass w2) => w1 -> w2 -> IO ();
    containerRemoveDestroy w1 w2 = do
    {
        containerRemove w1 w2;
        widgetDestroy w2;
    };

    switchView :: forall edit. Edit edit => (UISpec edit -> GCreateView edit) -> GeneralFunction edit (WholeEdit (UISpec edit)) -> GCreateView edit;
    switchView getview specfunc = do
    {
        box <- liftIO $ vBoxNew False 0;

        let
        {
            getVR :: UISpec edit -> View edit (GViewResult edit);
            getVR spec = getCompose $ getview spec;

            newWidgets :: GViewResult edit -> IO ();
            newWidgets vr = boxAddShow PackGrow box $ vrWidget vr;
        };

        firstvr <- liftOuter $ do
        {
            firstspec <- mapViewEdit (readOnlyGeneralLens specfunc) $ viewMutableRead $ \mr -> mr ReadWhole;
            getVR firstspec;
        };
        liftIO $ newWidgets firstvr;
        stateVar :: MVar (GViewResult edit) <- liftIO $ newMVar firstvr;

        unlift <- liftOuter $ liftIOView return;

        createViewAddAspect $ mvarStateAccess stateVar $ do
        {
            vr <- get;
            lift $ vrFirstAspect vr;
        };

        mapCreateViewEdit (readOnlyGeneralLens specfunc) $ createViewReceiveUpdate $ \_ (MkWholeEdit newspec) -> mvarStateAccess stateVar $ do
        {
            oldvr <- get;
            liftIO $ containerRemoveDestroy box $ vrWidget oldvr;
            newvr <- liftIO $ unlift $ getVR newspec;
            liftIO $ newWidgets newvr;
            put newvr;
        };

        createViewReceiveUpdates $ \mr edits -> mvarStateAccess stateVar $ do
        {
            vr <- get;
            lift $ vrUpdate vr mr edits;
        };

        return $ toWidget box;
    };

    switchGetView :: GetGView;
    switchGetView = MkGetView $ \getview uispec -> do
    {
        spec <- isUISpec uispec;
        return $ case spec of
        {
            MkUISwitch specfunc -> switchView getview specfunc;
        };
    };
}