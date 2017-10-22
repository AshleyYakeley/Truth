module Truth.UI.GTK.Useful where
{
    import Shapes;
    import Graphics.UI.Gtk;
    import Truth.Core;


    withSignalBlocked :: (GObjectClass obj) => ConnectId obj -> IO a -> IO a;
    withSignalBlocked conn = bracket_ (signalBlock conn) (signalUnblock conn);

    viewOn :: w -> Signal w (IO a) -> View edit a -> View edit (ConnectId w);
    viewOn widget signal v = liftIOView $ \unlift -> on widget signal $ unlift $ v;

    tryWithMVar :: MVar a -> (Maybe a -> IO b) -> IO b;
    tryWithMVar mv f = do
    {
        ma <- tryTakeMVar mv;
        finally (f ma) $ case ma of
        {
            Just a -> putMVar mv a;
            Nothing -> return ();
        };
    };

    ifMVar :: MVar () -> IO () -> IO ();
    ifMVar mv f = tryWithMVar mv $ \ma -> case ma of
    {
        Just _ -> f;
        _ -> return ();
    };

    listStoreTraverse_ :: MonadIO m => ListStore a -> (a -> m (Maybe a)) -> m ();
    listStoreTraverse_ store f = do
    {
        n <- liftIO $ listStoreGetSize store;
        for_ [0..(n-1)] $ \i -> do
        {
            oldval <- liftIO $ listStoreGetValue store i;
            mnewval <- f oldval;
            case mnewval of
            {
                Just newval -> liftIO $ listStoreSetValue store i newval;
                Nothing -> return ();
            };
        };
    };
}
