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

instance MonadTransUntrans DeferActionT where
    liftWithUntrans utmr =
        MkDeferActionT $
        liftWithUntrans $ \(MkWUntransFunction unlift) ->
            utmr $ MkWUntransFunction $ \(MkDeferActionT wma) -> unlift wma
    getDiscardingUntrans =
        MkDeferActionT $ do
            MkWUntransFunction du <- getDiscardingUntrans
            return $ MkWUntransFunction $ \(MkDeferActionT wma) -> du wma

deferAction :: Monad m => IO () -> DeferActionT m ()
deferAction action = MkDeferActionT $ tell [action]

runDeferActionT :: WUntransFunction DeferActionT
runDeferActionT =
    MkWUntransFunction $ \(MkDeferActionT (WriterT wma)) -> do
        (a, actions) <- wma
        for_ actions liftIO
        return a
