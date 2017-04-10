module Truth.UI.GTK.Useful where
{
    import Control.Concurrent.MVar;
    import Control.Exception;
    import Graphics.UI.Gtk;
    import Control.Monad.IOInvert;


    withSignalBlocked :: (GObjectClass obj) => ConnectId obj -> IO a -> IO a;
    withSignalBlocked conn = bracket_ (signalBlock conn) (signalUnblock conn);

    ifMVar :: MonadIOInvert m => MVar () -> m () -> m ();
    ifMVar mv f = liftIOInvert $ \unlift -> do
    {
        ma <- tryReadMVar mv;
        unlift $ case ma of
        {
            Just _ -> f;
            _ -> return ();
        };
    };
}
