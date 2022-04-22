module Control.Monad.Ology.General.Cont where

import Control.Monad.Ology.General.Trans.Constraint
import Control.Monad.Ology.General.Trans.Trans
import Control.Monad.Ology.General.Trans.Tunnel
import Import

class Monad m => MonadCont m where
    callCC :: ((a -> m b) -> m a) -> m a

instance (MonadTransTunnel t, MonadCont m, Monad (t m)) => MonadCont (t m) where
    callCC call =
        tunnel $ \unlift ->
            callCC $ \cont ->
                unlift $ call $ \a -> lift $ cont $ runIdentity $ unlift $ withTransConstraintTM @Monad $ return a
