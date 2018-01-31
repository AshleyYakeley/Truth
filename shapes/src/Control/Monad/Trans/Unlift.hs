module Control.Monad.Trans.Unlift where

import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Tunnel
import Shapes.Import

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
    -- | perform action without transformer effects (such as state change or output)
    impotent ::
           forall m a. Monad m
        => t m a
        -> t m a

class (MonadFail m, MonadIO m, MonadFix m) =>
      MonadUnliftIO m where
    liftIOWithUnlift :: forall r. ((forall a. m a -> IO a) -> IO r) -> m r
    -- | perform action without any transformer effects (such as state change or output)
    impotentIO :: forall a. m a -> m a

instance MonadUnliftIO IO where
    liftIOWithUnlift call = call id
    impotentIO = id

instance (MonadTransUnlift t, MonadUnliftIO m, MonadFail (t m), MonadIO (t m), MonadFix (t m)) =>
         MonadUnliftIO (t m) where
    liftIOWithUnlift call = liftWithUnlift $ \tmama -> liftIOWithUnlift $ \maioa -> call $ maioa . tmama
    impotentIO tma = impotent $ remonad impotentIO tma

instance MonadTransUnlift t => MonadTransConstraint MonadUnliftIO t where
    hasTransConstraint =
        withTransConstraintDict @MonadFail $ withTransConstraintDict @MonadIO $ withTransConstraintDict @MonadFix $ Dict

instance MonadTransUnlift IdentityT where
    liftWithUnlift call = IdentityT $ call $ runIdentityT
    impotent = id

instance MonadTransUnlift (ReaderT s) where
    liftWithUnlift call = ReaderT $ \s -> call $ \(ReaderT smr) -> smr s
    impotent = id

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
    impotent (WriterT mao) =
        WriterT $ do
            (a, _) <- mao
            return (a, mempty)

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
    impotent (StateT smas) =
        StateT $ \olds -> do
            (a, _) <- smas olds
            return (a, olds)
