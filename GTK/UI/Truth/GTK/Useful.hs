module UI.Truth.GTK.Useful where
{
    import Graphics.UI.Gtk;
    import System.Glib.Types;
    import Control.Exception;
    import Control.Concurrent.MVar;
    
    withSignalBlocked :: (GObjectClass obj) => ConnectId obj -> IO a -> IO a;
    withSignalBlocked conn = bracket_ (signalBlock conn) (signalUnblock conn);

    tryReadMVar :: MVar a -> IO (Maybe a);
    tryReadMVar mv = do
    {
        ma <- tryTakeMVar mv;
        case ma of
        {
            Just a -> putMVar mv a;
            _ -> return ();
        };
        return ma;
    };

    ifMVar :: MVar () -> IO a -> IO ();
    ifMVar mv f = do
    {
        ma <- tryReadMVar mv;
        case ma of
        {
            Just _ -> f >> return ();
            _ -> return ();
        };
    };
}
