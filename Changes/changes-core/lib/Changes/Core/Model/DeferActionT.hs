module Changes.Core.Model.DeferActionT
    ( DeferActionT
    , deferAction
    , runDeferActionT
    , deferActionResourceRunner
    ) where

import Changes.Core.Import
import Changes.Core.Resource

newtype DeferActionT m a =
    MkDeferActionT (WriterT [IO ()] m a)

deriving instance Functor m => Functor (DeferActionT m)

deriving instance Monad m => Applicative (DeferActionT m)

deriving instance Monad m => Monad (DeferActionT m)

deriving instance MonadFail m => MonadFail (DeferActionT m)

deriving instance MonadIO m => MonadIO (DeferActionT m)

deriving instance MonadFix m => MonadFix (DeferActionT m)

deriving instance MonadPlus m => Alternative (DeferActionT m)

deriving instance MonadPlus m => MonadPlus (DeferActionT m)

deriving instance MonadTrans DeferActionT

instance TransConstraint Functor DeferActionT where
    hasTransConstraint = Dict

instance TransConstraint Monad DeferActionT where
    hasTransConstraint = Dict

instance TransConstraint MonadFail DeferActionT where
    hasTransConstraint = Dict

instance TransConstraint MonadIO DeferActionT where
    hasTransConstraint = Dict

instance TransConstraint MonadFix DeferActionT where
    hasTransConstraint = Dict

instance TransConstraint MonadPlus DeferActionT where
    hasTransConstraint = Dict

deriving instance MonadTransTunnel DeferActionT

instance MonadTransUnlift DeferActionT where
    liftWithUnlift utmr = MkDeferActionT $ liftWithUnlift $ \unlift -> utmr $ \(MkDeferActionT wma) -> unlift wma
    getDiscardingUnlift =
        MkDeferActionT $ do
            MkWUnlift du <- getDiscardingUnlift
            return $ MkWUnlift $ \(MkDeferActionT wma) -> du wma

deferAction ::
       forall m. Monad m
    => IO ()
    -> DeferActionT m ()
deferAction action = MkDeferActionT $ tell [action]

runDeferActionT :: Unlift MonadTunnelIO DeferActionT
runDeferActionT (MkDeferActionT (WriterT wma)) = do
    (a, actions) <- wma
    for_ actions liftIO
    return a

deferActionResourceRunner ::
       forall m. MonadIO m
    => LifeCycleT m (ResourceRunner '[ DeferActionT])
deferActionResourceRunner = liftIO $ newResourceRunner runDeferActionT
