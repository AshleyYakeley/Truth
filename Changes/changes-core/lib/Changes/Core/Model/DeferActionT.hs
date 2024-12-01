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

deriving newtype instance Functor m => Functor (DeferActionT m)

deriving newtype instance Monad m => Applicative (DeferActionT m)

deriving newtype instance Monad m => Monad (DeferActionT m)

deriving newtype instance MonadFail m => MonadFail (DeferActionT m)

deriving newtype instance MonadIO m => MonadIO (DeferActionT m)

deriving newtype instance MonadFix m => MonadFix (DeferActionT m)

deriving newtype instance
         MonadPlus m => Alternative (DeferActionT m)

deriving newtype instance MonadPlus m => MonadPlus (DeferActionT m)

deriving newtype instance MonadTrans DeferActionT

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

deriving newtype instance MonadTransHoist DeferActionT

deriving newtype instance MonadTransTunnel DeferActionT

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
    => LifecycleT m (ResourceRunner '[ DeferActionT])
deferActionResourceRunner = liftIO $ newResourceRunner runDeferActionT
