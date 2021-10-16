module Control.Monad.Trans.Free where

import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Tunnel
import Control.Monad.Trans.Unlift
import Import

newtype FreeT m a = FreeT
    { runFreeT :: forall t. MonadTransUnliftAll t => t m a
    }

instance Functor m => Functor (FreeT m) where
    fmap ab (FreeT tma) = FreeT $ withTransConstraintTM @Functor $ fmap ab tma

instance Monad m => Applicative (FreeT m) where
    pure a = FreeT $ withTransConstraintTM @Monad $ pure a
    (FreeT tmab) <*> (FreeT tma) = FreeT $ withTransConstraintTM @Monad $ tmab <*> tma

instance Monad m => Monad (FreeT m) where
    return = pure
    (FreeT tma) >>= f = FreeT $ withTransConstraintTM @Monad $ tma >>= (runFreeT . f)

instance MonadFail m => MonadFail (FreeT m) where
    fail s = FreeT $ withTransConstraintTM @MonadFail $ fail s

instance MonadIO m => MonadIO (FreeT m) where
    liftIO ioa = FreeT $ withTransConstraintTM @MonadIO $ liftIO ioa

instance MonadFix m => MonadFix (FreeT m) where
    mfix afma = FreeT $ withTransConstraintTM @MonadFix $ mfix $ \a -> runFreeT $ afma a

instance MonadPlus m => Alternative (FreeT m) where
    empty = FreeT $ withTransConstraintTM @MonadPlus $ empty
    (FreeT ma) <|> (FreeT mb) = FreeT $ withTransConstraintTM @MonadPlus $ ma <|> mb

instance MonadPlus m => MonadPlus (FreeT m) where
    mzero = FreeT $ withTransConstraintTM @MonadPlus $ mzero
    mplus (FreeT ma) (FreeT mb) = FreeT $ withTransConstraintTM @MonadPlus $ mplus ma mb

instance MonadTrans FreeT where
    lift ma = FreeT $ lift ma

instance TransConstraint Functor FreeT where
    hasTransConstraint = Dict

instance TransConstraint Monad FreeT where
    hasTransConstraint = Dict

instance TransConstraint MonadFail FreeT where
    hasTransConstraint = Dict

instance TransConstraint MonadIO FreeT where
    hasTransConstraint = Dict

instance TransConstraint MonadFix FreeT where
    hasTransConstraint = Dict

instance TransConstraint MonadPlus FreeT where
    hasTransConstraint = Dict

instance MonadTransSemiTunnel FreeT

instance MonadTransTunnel FreeT where
    tunnel call = FreeT $ tunnel $ \tun -> call $ \(FreeT tm1r) -> tun tm1r

instance MonadTransUnlift FreeT

instance MonadTransUnliftAll FreeT where
    insideOut call = FreeT $ insideOut $ \unlift -> call $ unlift . runFreeT
    liftWithUnliftAll call = FreeT $ liftWithUnliftAll $ \unlift -> call $ \(FreeT tma) -> unlift tma
    getDiscardingUnliftAll =
        FreeT $
        withTransConstraintTM @Monad $ do
            MkWUnliftAll unlift <- getDiscardingUnliftAll
            return $ MkWUnliftAll $ \(FreeT tma) -> unlift tma

unliftFreeT :: WUnliftAll MonadUnliftIO FreeT
unliftFreeT = MkWUnliftAll $ \ft -> runIdentityT $ runFreeT ft
