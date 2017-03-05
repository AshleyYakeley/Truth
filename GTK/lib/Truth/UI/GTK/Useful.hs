module Truth.UI.GTK.Useful where
{
    import Control.Monad.IOInvert;
    import Graphics.UI.Gtk;
    import Control.Exception;
    import Control.Concurrent.MVar;

    withSignalBlocked :: (GObjectClass obj) => ConnectId obj -> IO a -> IO a;
    withSignalBlocked conn = bracket_ (signalBlock conn) (signalUnblock conn);

    ifMVar :: MonadIOInvert m => MVar () -> m a -> m ();
    ifMVar mv f = liftIOInvert $ \unlift -> do
    {
        ma <- tryReadMVar mv;
        unlift $ case ma of
        {
            Just _ -> f >> return ();
            _ -> return ();
        };
    };
}
