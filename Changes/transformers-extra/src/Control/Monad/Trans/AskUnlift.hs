module Control.Monad.Trans.AskUnlift where

import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Function
import Control.Monad.Trans.Tunnel
import Control.Monad.Trans.Unlift
import Data.Functor.One
import Import

-- | A transformer that has no effects (such as state change or output)
class MonadTransUnlift t => MonadTransAskUnlift t where
    askUnlift ::
           forall m. Monad m
        => t m (WUnliftT Monad t)
    default askUnlift :: forall m. (FunctorIdentity (Tunnel t), Monad m) => t m (WUnliftT Monad t)
    askUnlift = tunnel $ \unlift -> pure $ fpure $ MkWUnliftT $ \tma -> fmap fextract $ unlift tma

-- | A monad that has no effects over IO (such as state change or output)
class MonadUnliftIO m => MonadAskUnliftIO m where
    askUnliftIO :: m (WIOFunction m)
    askUnliftIO = tunnelIO $ \unlift -> pure $ fpure $ MkWMFunction $ \ma -> fmap fextract $ unlift ma

instance MonadAskUnliftIO IO where
    askUnliftIO = return $ MkWMFunction id

instance (MonadTransAskUnlift t, MonadAskUnliftIO m, MonadFail (t m), MonadIO (t m), MonadFix (t m)) =>
             MonadAskUnliftIO (t m) where
    askUnliftIO = do
        MkWUnliftT unlift <- askUnlift
        MkWMFunction unliftIO <- lift askUnliftIO
        return $ MkWMFunction $ unliftIO . unlift

instance MonadTransAskUnlift t => TransConstraint MonadAskUnliftIO t where
    hasTransConstraint =
        withTransConstraintDict @MonadFail $ withTransConstraintDict @MonadIO $ withTransConstraintDict @MonadFix $ Dict

instance MonadTransAskUnlift IdentityT

instance MonadTransAskUnlift (ReaderT s)

contractT ::
       forall (t :: TransKind) m. (MonadTransAskUnlift t, Monad m)
    => MFunction (t (t m)) (t m)
contractT ttma =
    case hasTransConstraint @Monad @t @m of
        Dict -> do
            MkWUnliftT unlift <- askUnlift
            unlift ttma

contractTBack ::
       forall (t :: TransKind) m. (MonadTransAskUnlift t, Monad m)
    => MBackFunction (t (t m)) (t m)
contractTBack call =
    case hasTransConstraint @Monad @t @m of
        Dict -> contractT $ call lift
