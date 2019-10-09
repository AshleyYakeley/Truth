module Control.Monad.Trans.Free where

import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Tunnel
import Control.Monad.Trans.Unlift
import Data.Constraint
import Prelude hiding (fail)

newtype FreeT m a = FreeT
    { runFreeT :: forall t. MonadTransUntrans t => t m a
    }

instance Monad m => Functor (FreeT m) where
    fmap ab (FreeT tma) = FreeT $ withTransConstraintTM @Monad $ fmap ab tma

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

instance MonadTransConstraint Monad FreeT where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadFail FreeT where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadIO FreeT where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadFix FreeT where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadPlus FreeT where
    hasTransConstraint = Dict

instance MonadTransSemiTunnel FreeT

instance MonadTransTunnel FreeT where
    tunnel call = FreeT $ tunnel $ \tun -> call $ \(FreeT tm1r) -> tun tm1r
    transExcept (FreeT txa) = FreeT $ transExcept txa

instance MonadTransUnlift FreeT

instance MonadTransUntrans FreeT where
    liftWithUntrans call = FreeT $ liftWithUntrans $ \unlift -> call $ \(FreeT tma) -> unlift tma
    getDiscardingUntrans =
        FreeT $
        withTransConstraintTM @Monad $ do
            MkWUntrans unlift <- getDiscardingUntrans
            return $ MkWUntrans $ \(FreeT tma) -> unlift tma

unliftFreeT :: WUntrans FreeT
unliftFreeT = MkWUntrans $ \ft -> identityUntrans $ runFreeT ft
