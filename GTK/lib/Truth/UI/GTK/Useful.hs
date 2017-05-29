module Truth.UI.GTK.Useful where
{
    import Data.Functor.Identity;
    import Control.Concurrent.MVar;
    import Control.Exception;
    import Graphics.UI.Gtk;
    import Control.Monad.Trans.Class;
    import Control.Monad.IOInvert;


    withSignalBlocked :: (GObjectClass obj) => ConnectId obj -> IO a -> IO a;
    withSignalBlocked conn = bracket_ (signalBlock conn) (signalUnblock conn);

    withMVar' :: MonadIOInvert m => MVar a -> (a -> m ()) -> m ();
    withMVar' mv f1 = mapIOInvert (\f2 -> withMVar mv $ fmap (fmap runIdentity) f2) $ fmap (fmap Identity) f1;

    ifMVar :: MonadIOInvert m => MVar () -> m () -> m ();
    ifMVar mv f = liftIOInvert $ \unlift -> do
    {
        ma <- lift $ tryReadMVar mv;
        unlift $ case ma of
        {
            Just _ -> f;
            _ -> return ();
        };
    };
}
