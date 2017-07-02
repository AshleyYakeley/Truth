module Control.Monad.IsStateIO where
{
    import Data.Kind;
    import Control.Exception;
    import Control.Concurrent.MVar;
    import Control.Monad.Fix;
    import Control.Monad.IO.Class;
    import Control.Monad.Trans.State.Extra;
    import Control.Monad.Tunnel;


    class (MonadFix m,MonadIO m) => IsStateIO m where
    {
        type IOState m :: Type;
        runStateIO :: forall a. m a -> IOState m -> IO (a,IOState m);
        mkStateIO :: forall a. (IOState m -> IO (a,IOState m)) -> m a;
    };

    instance IsStateIO IO where
    {
        type IOState IO = ();
        runStateIO ioa _ = fmap (\a -> (a,())) ioa;
        mkStateIO s = fmap fst $ s ();
    };

    instance IsStateIO m => IsStateIO (StateT s m) where
    {
        type IOState (StateT s m) = (s,IOState m);
        runStateIO (StateT smas) (s,ss) = fmap swap3' $ runStateIO (smas s) ss;
        mkStateIO ssioass = StateT $ \s -> mkStateIO $ \ss -> fmap swap3 $ ssioass (s,ss);
    };

    toStateIO :: IsStateIO m => StateT (IOState m) IO a -> m a;
    toStateIO (StateT smas) = mkStateIO smas;

    fromStateIO :: IsStateIO m => m a -> StateT (IOState m) IO a;
    fromStateIO ma = StateT $ runStateIO ma;

    mapIOInvert :: (Functor f,Functor g,IsStateIO m) => (forall a. f (IO (g a)) -> IO a) -> f (m (g b)) -> m b;
    mapIOInvert ff fmgb = mkStateIO $ \oldstate -> ff $ fmap (\mgb -> fmap (\(gb,s) -> fmap (\b -> (b,s)) gb) $ runStateIO mgb oldstate) fmgb;

    tunnelStateIO :: IsStateIO m => (forall a. (m r -> IO a) -> IO a) -> m r;
    tunnelStateIO call = toStateIO $ tunnel $ \unlift -> call $ \mr -> unlift $ fromStateIO mr;

    mvarStateAccess :: IsStateIO m => MVar s -> StateAccess m s;
    mvarStateAccess mvar sma = mkStateIO $ \oldios -> modifyMVar mvar $ \olds -> do
    {
        (a,(news,newios)) <- runStateIO sma (olds,oldios);
        return (news,(a,newios));
    };

    tryModifyMVar :: MVar a -> (a -> IO (a,b)) -> IO (Maybe b);
    tryModifyMVar var call = mask $ \restore -> do
    {
        molda <- tryTakeMVar var;
        case molda of
        {
            Just olda -> do
            {
                (newa,b) <- restore (call olda >>= evaluate) `onException` putMVar var olda;
                putMVar var newa;
                return $ Just b;
            };
            Nothing -> return Nothing;
        };
    };

    mvarTryStateT :: IsStateIO m => MVar s -> StateT s m r -> m (Maybe r);
    mvarTryStateT var call = mkStateIO $ \oldios -> do
    {
        mrs <- tryModifyMVar var $ \olds -> do
        {
            (a,(news,newios)) <- runStateIO call (olds,oldios);
            return (news,(a,newios));
        };
        return $ case mrs of
        {
            Nothing -> (Nothing,oldios);
            Just (r,newios) -> (Just r,newios);
        }
    };

{-
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
-}


}
