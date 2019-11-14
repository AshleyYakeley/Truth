module Control.Monad.Trans.AskUnlift where

import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Function
import Control.Monad.Trans.Unlift
import Import

-- | A transformer that has no effects (such as state change or output)
class MonadTransUnliftAll t => MonadTransAskUnlift t where
    askUnlift ::
           forall m. Monad m
        => t m (WUnliftAll Monad t)

-- | A monad that has no effects over IO (such as state change or output)
class MonadUnliftIO m => MonadAskUnliftIO m where
    askUnliftIO :: m (WIOFunction m)

instance MonadAskUnliftIO IO where
    askUnliftIO = return $ MkWMFunction id

instance (MonadTransAskUnlift t, MonadAskUnliftIO m, MonadFail (t m), MonadIO (t m), MonadFix (t m)) =>
             MonadAskUnliftIO (t m) where
    askUnliftIO = do
        MkWUnliftAll unlift <- askUnlift
        MkWMFunction unliftIO <- lift askUnliftIO
        return $ MkWMFunction $ unliftIO . unlift

instance MonadTransAskUnlift t => MonadTransConstraint MonadAskUnliftIO t where
    hasTransConstraint =
        withTransConstraintDict @MonadFail $ withTransConstraintDict @MonadIO $ withTransConstraintDict @MonadFix $ Dict

instance MonadTransAskUnlift IdentityT where
    askUnlift = return identityWUnliftAll

instance MonadTransAskUnlift (ReaderT s) where
    askUnlift = do
        s <- ask
        return $ MkWUnliftAll $ \mr -> runReaderT mr s

contractT ::
       forall (t :: TransKind) m. (MonadTransAskUnlift t, Monad m)
    => MFunction (t (t m)) (t m)
contractT ttma =
    case hasTransConstraint @Monad @t @m of
        Dict -> do
            MkWUnliftAll unlift <- askUnlift
            unlift ttma
