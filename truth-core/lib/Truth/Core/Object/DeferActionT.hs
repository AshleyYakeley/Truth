module Truth.Core.Object.DeferActionT
    ( DeferActionT
    , deferActionT
    , runDeferActionT
    ) where

import Truth.Core.Import
import Truth.Debug.Object

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

deriving instance MonadTransTunnel DeferActionT

instance MonadTransUnlift DeferActionT where
    liftWithUnlift utmr =
        MkDeferActionT $ liftWithUnlift $ \(MkUnlift unlift) -> utmr $ MkUnlift $ \(MkDeferActionT wma) -> unlift wma
    getDiscardingUnlift =
        MkDeferActionT $ tracePureBracket "DeferActionT.getDiscardingUnlift" $ do
            MkUnlift du <- getDiscardingUnlift
            return $ MkUnlift $ \(MkDeferActionT wma) -> du wma

deferActionT :: MonadIO m => IO () -> DeferActionT m ()
deferActionT action = traceBracket "deferActionT" $ MkDeferActionT $ tell [action]

runDeferActionT :: Unlift DeferActionT
runDeferActionT = traceThing "runDeferActionT" $
    MkUnlift $ \(MkDeferActionT (WriterT wma)) -> do
        (a, actions) <- wma
        traceBracket "runDeferActionT: actions" $ for_ actions liftIO
        return a
