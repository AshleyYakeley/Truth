module Control.Monad.Ology.Trans.AskUnlift where

import Control.Monad.Ology.Function
import Control.Monad.Ology.MonadExtract
import Control.Monad.Ology.MonadIdentity
import Control.Monad.Ology.Trans.Constraint
import Control.Monad.Ology.Trans.Tunnel
import Control.Monad.Ology.Trans.Unlift
import Import

-- | A transformer that has no effects (such as state change or output)
class MonadTransUnlift t => MonadTransAskUnlift t where
    askUnlift ::
           forall m. Monad m
        => t m (WUnlift Monad t)
    default askUnlift :: forall m. (MonadIdentity (Tunnel t), Monad m) => t m (WUnlift Monad t)
    askUnlift = tunnel $ \unlift -> pure $ pure $ MkWUnlift $ \tma -> fmap mToValue $ unlift tma

-- | A monad that has no effects over IO (such as state change or output)
class MonadUnliftIO m => MonadAskUnliftIO m where
    askUnliftIO :: m (WMFunction m IO)
    askUnliftIO = tunnelIO $ \unlift -> pure $ pure $ MkWMFunction $ \ma -> fmap mToValue $ unlift ma

instance MonadAskUnliftIO IO where
    askUnliftIO = return $ MkWMFunction id

instance (MonadTransAskUnlift t, MonadAskUnliftIO m, MonadFail (t m), MonadIO (t m), MonadFix (t m)) =>
             MonadAskUnliftIO (t m) where
    askUnliftIO = do
        MkWUnlift unlift <- askUnlift
        MkWMFunction unliftIO <- lift askUnliftIO
        return $ MkWMFunction $ unliftIO . unlift

instance MonadTransAskUnlift t => TransConstraint MonadAskUnliftIO t where
    hasTransConstraint =
        withTransConstraintDict @MonadFail $ withTransConstraintDict @MonadIO $ withTransConstraintDict @MonadFix $ Dict

instance MonadTransAskUnlift IdentityT

instance MonadTransAskUnlift (ReaderT s)

contractT ::
       forall (t :: TransKind) m. (MonadTransAskUnlift t, Monad m)
    => t (t m) --> t m
contractT ttma =
    case hasTransConstraint @Monad @t @m of
        Dict -> do
            MkWUnlift unlift <- askUnlift
            unlift ttma

contractTBack ::
       forall (t :: TransKind) m. (MonadTransAskUnlift t, Monad m)
    => t (t m) -/-> t m
contractTBack call =
    case hasTransConstraint @Monad @t @m of
        Dict -> contractT $ call lift
