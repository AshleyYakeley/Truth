module Truth.Core.Object.DeferActionT
    ( DeferActionT
    , deferAction
    , runDeferActionT
    ) where

import Truth.Core.Import

newtype DeferActionT m a =
    MkDeferActionT (WriterT [IO ()] m a)

deriving instance Monad m => Functor (DeferActionT m)

deriving instance Monad m => Applicative (DeferActionT m)

deriving instance Monad m => Monad (DeferActionT m)

deriving instance MonadFail m => MonadFail (DeferActionT m)

deriving instance MonadIO m => MonadIO (DeferActionT m)

deriving instance MonadFix m => MonadFix (DeferActionT m)

deriving instance MonadPlus m => Alternative (DeferActionT m)

deriving instance MonadPlus m => MonadPlus (DeferActionT m)

deriving instance MonadTrans DeferActionT

instance MonadTransConstraint Monad DeferActionT where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadFail DeferActionT where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadIO DeferActionT where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadFix DeferActionT where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadPlus DeferActionT where
    hasTransConstraint = Dict

instance MonadTransSemiTunnel DeferActionT

deriving instance MonadTransTunnel DeferActionT

instance MonadTransUnlift DeferActionT

instance MonadTransUnliftAll DeferActionT where
    liftWithUnliftAll utmr = MkDeferActionT $ liftWithUnliftAll $ \unlift -> utmr $ \(MkDeferActionT wma) -> unlift wma
    getDiscardingUnliftAll =
        MkDeferActionT $ do
            MkWUnliftAll du <- getDiscardingUnliftAll
            return $ MkWUnliftAll $ \(MkDeferActionT wma) -> du wma

deferAction :: Monad m => IO () -> DeferActionT m ()
deferAction action = MkDeferActionT $ tell [action]

runDeferActionT :: UnliftAll DeferActionT
runDeferActionT (MkDeferActionT (WriterT wma)) = do
    (a, actions) <- wma
    for_ actions liftIO
    return a
