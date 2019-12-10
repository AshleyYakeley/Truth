module Truth.Core.Object.DeferActionT
    ( DeferActionT
    , deferAction
    , runDeferActionT
    , deferActionResourceRunner
    ) where

import Truth.Core.Import
import Truth.Core.Resource
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

instance MonadTransSemiTunnel DeferActionT

deriving instance MonadTransTunnel DeferActionT

instance MonadTransUnlift DeferActionT

instance MonadTransUnliftAll DeferActionT where
    insideOut call = MkDeferActionT $ insideOut $ \unlift -> call $ \(MkDeferActionT wma) -> unlift wma
    liftWithUnliftAll utmr = MkDeferActionT $ liftWithUnliftAll $ \unlift -> utmr $ \(MkDeferActionT wma) -> unlift wma
    getDiscardingUnliftAll =
        MkDeferActionT $ tracePureBracket "DeferActionT.getDiscardingUnlift" $ do
            MkWUnliftAll du <- getDiscardingUnliftAll
            return $ MkWUnliftAll $ \(MkDeferActionT wma) -> du wma

deferAction ::
       forall m. MonadIO m
    => IO ()
    -> DeferActionT m ()
deferAction action = traceBracket "deferAction" $ MkDeferActionT $ tell [action]

runDeferActionT :: UnliftAll MonadUnliftIO DeferActionT
runDeferActionT (MkDeferActionT (WriterT wma)) = traceBracket "runDeferActionT" $ do
    (a, actions) <- traceBracket "runDeferActionT.body" $ wma
    traceBracket "runDeferActionT.deferred" $ for_ actions liftIO
    return a

deferActionResourceRunner :: LifeCycleIO (ResourceRunner '[ DeferActionT])
deferActionResourceRunner = liftIO $ newResourceRunner runDeferActionT
