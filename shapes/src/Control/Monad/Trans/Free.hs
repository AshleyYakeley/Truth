module Control.Monad.Trans.Free where

import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Tunnel
import Control.Monad.Trans.Unlift
import Shapes.Import

newtype FreeT m a = FreeT
    { runFreeT :: forall t. MonadTransUnlift t =>
                                   t m a
    }

instance Monad m => Functor (FreeT m) where
    fmap ab (FreeT tma) = FreeT $ withTransConstraintTM @Monad $ fmap ab tma

instance Monad m => Applicative (FreeT m) where
    pure a = FreeT $ withTransConstraintTM @Monad $ pure a
    (FreeT tmab) <*> (FreeT tma) = FreeT $ withTransConstraintTM @Monad $ tmab <*> tma

instance Monad m => Monad (FreeT m) where
    return = pure
    (FreeT tma) >>= f = FreeT $ withTransConstraintTM @Monad $ tma >>= (runFreeT . f)

instance MonadIO m => MonadIO (FreeT m) where
    liftIO ioa = FreeT $ withTransConstraintTM @MonadIO $ liftIO ioa

instance MonadTrans FreeT where
    lift ma = FreeT $ lift ma

instance MonadTransConstraint Monad FreeT where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadIO FreeT where
    hasTransConstraint = Dict

instance MonadTransTunnel FreeT where
    tunnel call = FreeT $ tunnel $ \tun -> call $ \(FreeT tm1r) -> tun tm1r
    transExcept (FreeT txa) = FreeT $ transExcept txa

instance MonadTransUnlift FreeT where
    liftWithUnlift call = FreeT $ liftWithUnlift $ \unlift -> call $ \(FreeT tma) -> unlift tma
    impotent (FreeT tma) = FreeT $ impotent tma
