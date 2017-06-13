module Control.Monad.IOInvert where
{
    import Data.Kind;
    import Control.Monad.Fix;
    import Control.Monad.IO.Class;
    import Control.Monad.Trans.State;


    swap3 :: (a,(b,c)) -> ((a,b),c);
    swap3 (a,(b,c)) = ((a,b),c);

    swap3' :: ((a,b),c) -> (a,(b,c));
    swap3' ((a,b),c) = (a,(b,c));

    stateFst :: Functor m => StateT s m a -> StateT (s,s') m a;
    stateFst (StateT sma) = StateT $ \(olds,s') -> fmap (\(a,news) -> (a,(news,s'))) $ sma olds;

    stateSnd :: Functor m => StateT s m a -> StateT (s',s) m a;
    stateSnd (StateT sma) = StateT $ \(s',olds) -> fmap (\(a,news) -> (a,(s',news))) $ sma olds;

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
}
