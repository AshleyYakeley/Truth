module Changes.Core.Model.DeferActionT
    ( DeferActionT
    , deferAction
    , runDeferActionT
    , deferActionResourceRunner
    ) where

import Changes.Core.Import
import Changes.Core.Resource
import Changes.Debug.Reference

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

deriving instance MonadTransHoist DeferActionT

deriving instance MonadTransTunnel DeferActionT

instance MonadTransUnlift DeferActionT where
    liftWithUnlift utmr = MkDeferActionT $ liftWithUnlift $ \unlift -> utmr $ \(MkDeferActionT wma) -> unlift wma
    getDiscardingUnlift =
        MkDeferActionT $ tracePureBracket "DeferActionT.getDiscardingUnlift" $ do
            MkWUnlift du <- getDiscardingUnlift
            return $ MkWUnlift $ \(MkDeferActionT wma) -> du wma

deferAction ::
       forall m. MonadIO m
    => IO ()
    -> DeferActionT m ()
deferAction action = traceBracket "deferAction" $ MkDeferActionT $ tell [action]

runDeferActionT :: Unlift MonadTunnelIO DeferActionT
runDeferActionT (MkDeferActionT (WriterT wma)) = traceBracket "runDeferActionT" $ do
    (a, actions) <- traceBracket "runDeferActionT.body" $ wma
    traceBracket "runDeferActionT.deferred" $ for_ actions liftIO
    return a

deferActionResourceRunner ::
       forall m. MonadIO m
    => LifeCycleT m (ResourceRunner '[ DeferActionT])
deferActionResourceRunner = liftIO $ newResourceRunner runDeferActionT
