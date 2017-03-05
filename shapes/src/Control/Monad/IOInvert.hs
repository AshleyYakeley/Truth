module Control.Monad.IOInvert where
{
    import Control.Monad.IO.Class;
    import Control.Monad.Trans.State;


    class MonadIO m => MonadIOInvert m where
    {
        liftIOInvert :: forall r. (forall o. (forall a. m a -> IO (o,a)) -> IO (o,r)) -> m r;
    };

    instance MonadIOInvert IO where
    {
        liftIOInvert iaioaior = fmap snd $ iaioaior (fmap (\a -> ((),a)));
    };

    instance (MonadIOInvert m) => MonadIOInvert (StateT s m) where
    {
        liftIOInvert smaioaior = StateT $ \oldstate -> liftIOInvert $ \maioa -> fmap swap3 $ smaioaior $ \(StateT smas) -> fmap swap3' $ maioa $ smas oldstate where
        {
            swap3 ((o,s),r) = (o,(r,s));
            swap3' (o,(a,s)) = ((o,s),a);
        }
    };
}
