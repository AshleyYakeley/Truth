module Truth.UI.GTK.Useful where
{
    import Shapes;
    import Graphics.UI.Gtk;


    withSignalBlocked :: (GObjectClass obj) => ConnectId obj -> IO a -> IO a;
    withSignalBlocked conn = bracket_ (signalBlock conn) (signalUnblock conn);

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
}
