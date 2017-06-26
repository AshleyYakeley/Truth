module Control.Monad.IOInvert where
{
    import Data.Kind;
    import Data.Functor.Identity;
    import Control.Monad.Fix;
    import Control.Monad.IO.Class;
    import Control.Monad.Trans.State;
    import Data.Lens;


    unitStateT :: Functor m => m a -> StateT () m a;
    unitStateT ma = StateT $ \() -> fmap (\a -> (a,())) ma;

    runUnitStateT :: Functor m => StateT () m a -> m a;
    runUnitStateT sma = fmap fst $ runStateT sma ();

    swap3 :: (a,(b,c)) -> ((a,b),c);
    swap3 (a,(b,c)) = ((a,b),c);

    swap3' :: ((a,b),c) -> (a,(b,c));
    swap3' ((a,b),c) = (a,(b,c));

    revertStateT :: Monad m => StateT s m a -> StateT s m a;
    revertStateT sma = do
    {
        s <- get;
        a <- sma;
        put s;
        return a;
    };

    swapStateT :: Functor m => StateT s1 (StateT s2 m) a -> StateT s2 (StateT s1 m) a;
    swapStateT (StateT s1s2a) = StateT $ \olds2 -> StateT $ \olds1 -> fmap (\((a,news1),news2) -> ((a,news2),news1)) $ runStateT (s1s2a olds1) olds2;

    joinStateT :: Functor m => StateT s1 (StateT s2 m) a -> StateT (s1,s2) m a;
    joinStateT (StateT s1s2a) = StateT $ \(olds1,olds2) -> fmap swap3' $ runStateT (s1s2a olds1) olds2;

    splitStateT :: Functor m => StateT (s1,s2) m a -> StateT s1 (StateT s2 m) a;
    splitStateT (StateT s1s2a) = StateT $ \olds1 -> StateT $ \olds2 -> fmap swap3 $ s1s2a (olds1,olds2);

    lensStateT :: Functor m => Lens' Identity whole part -> StateT part m a -> StateT whole m a;
    lensStateT MkLens{..} (StateT pma) = StateT $ \oldw -> fmap (\(a,newp) -> (a,runIdentity $ lensPutback newp oldw)) $ pma (lensGet oldw);

    stateFst :: Functor m => StateT s m a -> StateT (s,s') m a;
    stateFst = lensStateT fstLens;

    stateSnd :: Functor m => StateT s m a -> StateT (s',s) m a;
    stateSnd = lensStateT sndLens;

    tunnelStateT :: (forall a. (forall m1. StateT s m1 r -> m1 a) -> m2 a) -> StateT s m2 r;
    tunnelStateT ff = StateT $ \olds -> ff $ \(StateT smrs) -> smrs olds;

    withMStateT :: (forall a. m1 a -> m2 a) -> StateT s m1 r -> StateT s m2 r;
    withMStateT mma sm1 = tunnelStateT $ \tunnel -> mma $ tunnel sm1;

    isoStateT :: Functor m => (s1 -> s2, s2 -> s1) -> StateT s1 m r -> StateT s2 m r;
    isoStateT (s1s2,s2s1) (StateT s1mrs1) = StateT $ \olds2 -> fmap (\(r,news1) -> (r,s1s2 news1)) $ s1mrs1 $ s2s1 olds2;

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
