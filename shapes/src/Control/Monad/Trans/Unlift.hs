module Control.Monad.Trans.Unlift where

import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Tunnel
import Shapes.Import

class (MonadTransConstraint MonadIO t, MonadTransTunnel t) =>
      MonadTransUnlift t where
    liftWithUnlift ::
           forall m r. MonadUnliftIO m
        => ((forall a. t m a -> m a) -> m r)
        -> t m r

class MonadIO m =>
      MonadUnliftIO m where
    liftIOWithUnlift :: forall r. ((forall a. m a -> IO a) -> IO r) -> m r

instance MonadUnliftIO IO where
    liftIOWithUnlift call = call id

instance (MonadTransUnlift t, MonadUnliftIO m, MonadIO (t m)) => MonadUnliftIO (t m) where
    liftIOWithUnlift call = liftWithUnlift $ \tmama -> liftIOWithUnlift $ \maioa -> call $ maioa . tmama

instance MonadTransUnlift IdentityT where
    liftWithUnlift call = IdentityT $ call $ runIdentityT

instance MonadTransUnlift (ReaderT s) where
    liftWithUnlift call = ReaderT $ \s -> call $ \(ReaderT smr) -> smr s

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
