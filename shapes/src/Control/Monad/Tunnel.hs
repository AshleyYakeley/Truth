module Control.Monad.Tunnel where
{
    import Control.Monad.Trans.Class;
    import Control.Monad.Trans.State;
    import Control.Monad.Trans.Reader;
    import Control.Monad.Trans.Writer;


    class MonadTrans t => MonadTunnel t where
    {
        tunnel :: (forall a. (forall m1. t m1 r -> m1 a) -> m2 a) -> t m2 r;
    };

    remonad :: MonadTunnel t => (forall a. m1 a -> m2 a) -> t m1 r -> t m2 r;
    remonad mma sm1 = tunnel $ \tun -> mma $ tun sm1;

    instance MonadTunnel (StateT s) where
    {
        tunnel call = StateT $ \olds -> call $ \(StateT smrs) -> smrs olds;
    };

    instance MonadTunnel (ReaderT s) where
    {
        tunnel call = ReaderT $ \s -> call $ \(ReaderT smr) -> smr s;
    };

    instance Monoid s => MonadTunnel (WriterT s) where
    {
        tunnel call = WriterT $ call $ \(WriterT mrs) -> mrs;
    };
}
