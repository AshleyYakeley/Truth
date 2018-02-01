module Control.Monad.Trans.Unlift where

import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Tunnel
import Shapes.Import

newtype Unlift (t :: (* -> *) -> * -> *) = MkUnlift
    { runUnlift :: forall (m :: * -> *) (r :: *). MonadUnliftIO m =>
                                                      t m r -> m r
    }

identityUnlift :: Unlift IdentityT
identityUnlift = MkUnlift runIdentityT

mvarRun :: MonadUnliftIO m => MVar s -> StateT s m r -> m r
mvarRun var (StateT smr) = liftIOWithUnlift $ \unlift -> modifyMVar var $ \olds -> unlift $ fmap swap $ smr olds

mvarUnlift :: MVar s -> Unlift (StateT s)
mvarUnlift var = MkUnlift $ mvarRun var

class ( MonadTransConstraint MonadFail t
      , MonadTransConstraint MonadIO t
      , MonadTransConstraint MonadFix t
      , MonadTransConstraint MonadPlus t
      , MonadTransTunnel t
      ) =>
      MonadTransUnlift t where
    liftWithUnlift ::
           forall m r. MonadUnliftIO m
        => ((forall a. t m a -> m a) -> m r)
        -> t m r
    -- ^ lift with an unlifting function that accounts for the transformer's effects (using MVars where necessary)
    -- | return an 'Unlift' that discards the transformer's effects (such as state change or output)
    getDiscardingUnlift ::
           forall m. Monad m
        => t m (Unlift t)

newtype UnliftIO m = MkUnliftIO
    { runUnliftIO :: forall r. m r -> IO r
    }

remonadUnliftIO :: MonadTransTunnel t => (forall a. m1 a -> m2 a) -> UnliftIO (t m2) -> UnliftIO (t m1)
remonadUnliftIO ff (MkUnliftIO r2) = MkUnliftIO $ \m1a -> r2 $ remonad ff m1a

mvarUnliftIO :: MVar s -> UnliftIO (StateT s IO)
mvarUnliftIO var = MkUnliftIO $ mvarRun var

class (MonadFail m, MonadIO m, MonadFix m) =>
      MonadUnliftIO m where
    liftIOWithUnlift :: forall r. ((forall a. m a -> IO a) -> IO r) -> m r
    -- ^ lift with an unlifting function that accounts for all transformer effects
    -- | return an 'UnliftIO' that discards all transformer effects (such as state change or output)
    getDiscardingUnliftIO :: m (UnliftIO m)

instance MonadUnliftIO IO where
    liftIOWithUnlift call = call id
    getDiscardingUnliftIO = return $ MkUnliftIO id

instance (MonadTransUnlift t, MonadUnliftIO m, MonadFail (t m), MonadIO (t m), MonadFix (t m)) =>
         MonadUnliftIO (t m) where
    liftIOWithUnlift call = liftWithUnlift $ \tmama -> liftIOWithUnlift $ \maioa -> call $ maioa . tmama
    getDiscardingUnliftIO = do
        MkUnlift unlift <- getDiscardingUnlift
        MkUnliftIO unliftIO <- lift getDiscardingUnliftIO
        return $ MkUnliftIO $ unliftIO . unlift

instance MonadTransUnlift t => MonadTransConstraint MonadUnliftIO t where
    hasTransConstraint =
        withTransConstraintDict @MonadFail $ withTransConstraintDict @MonadIO $ withTransConstraintDict @MonadFix $ Dict

instance MonadTransUnlift IdentityT where
    liftWithUnlift call = IdentityT $ call $ runIdentityT
    getDiscardingUnlift = return identityUnlift

instance MonadTransUnlift (ReaderT s) where
    liftWithUnlift call = ReaderT $ \s -> call $ \(ReaderT smr) -> smr s
    getDiscardingUnlift = do
        s <- ask
        return $ MkUnlift $ \mr -> runReaderT mr s

instance Monoid s => MonadTransUnlift (WriterT s) where
    liftWithUnlift call = do
        var <- liftIO $ newMVar mempty
        r <-
            lift $
            call $ \(WriterT mrs) -> do
                (r, output) <- mrs
                liftIO $ modifyMVar var $ \oldoutput -> return (mappend oldoutput output, ())
                return r
        totaloutput <- liftIO $ takeMVar var
        tell totaloutput
        return r
    getDiscardingUnlift =
        return $
        MkUnlift $ \mr -> do
            (r, _discarded) <- runWriterT mr
            return r

instance MonadTransUnlift (StateT s) where
    liftWithUnlift call = do
        initialstate <- get
        var <- liftIO $ newMVar initialstate
        r <-
            lift $
            call $ \(StateT sma) ->
                liftIOWithUnlift $ \unlift -> modifyMVar var $ \oldstate -> unlift $ fmap swap $ sma oldstate
        finalstate <- liftIO $ takeMVar var
        put finalstate
        return r
    getDiscardingUnlift = do
        s <- get
        return $
            MkUnlift $ \mr -> do
                (r, _discarded) <- runStateT mr s
                return r
