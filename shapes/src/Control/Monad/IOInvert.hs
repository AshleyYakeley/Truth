module Control.Monad.IOInvert where
{
    import Control.Monad.IO.Class;
    import Control.Monad.Trans.Class;
    import Control.Monad.Trans.State;


    swap3 :: (a,(b,c)) -> ((a,b),c);
    swap3 (a,(b,c)) = ((a,b),c);

    swap3' :: ((a,b),c) -> (a,(b,c));
    swap3' ((a,b),c) = (a,(b,c));

    stateFst :: Functor m => StateT s m a -> StateT (s,s') m a;
    stateFst (StateT sma) = StateT $ \(olds,s') -> fmap (\(a,news) -> (a,(news,s'))) $ sma olds;

    stateSnd :: Functor m => StateT s m a -> StateT (s',s) m a;
    stateSnd (StateT sma) = StateT $ \(s',olds) -> fmap (\(a,news) -> (a,(s',news))) $ sma olds;

    class MonadIO m => MonadIOInvert m where
    {
        liftIOInvert :: forall r. (forall s. (forall a. m a -> StateT s IO a) -> StateT s IO r) -> m r;
    };

    instance MonadIOInvert IO where
    {
        liftIOInvert iauaur = evalStateT (iauaur lift) ();
    };

    instance MonadIOInvert m => MonadIOInvert (StateT state m) where
    {
        liftIOInvert iasasr = StateT $ \oldstate -> liftIOInvert $ \masa -> StateT $ \olds -> fmap swap3 $ runStateT (iasasr $ buildStateT masa) (oldstate,olds) where
        {
            buildStateT :: (forall b. m b -> StateT s IO b) -> StateT state m a -> StateT (state, s) IO a;
            buildStateT mbsb (StateT stmas) = StateT $ \(oldstate,olds) -> fmap swap3' $ runStateT (mbsb (stmas oldstate)) olds;
        };
    };

    mapIOInvert :: (Functor f,Functor g,MonadIOInvert m) => (forall a. f (IO (g a)) -> IO a) -> f (m (g b)) -> m b;
    mapIOInvert ff fmgb = liftIOInvert $ \unlift -> StateT $ \oldstate -> ff $ fmap (\mgb -> fmap (\(gb,s) -> fmap (\b -> (b,s)) gb) $ runStateT (unlift mgb) oldstate) fmgb;
}
