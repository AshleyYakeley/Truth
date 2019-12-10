module Control.Monad.LifeCycleIO where

import Control.Monad.Trans.LifeCycle
import Shapes.Import

type LifeCycleIO = LifeCycleT IO

class MonadIO m => MonadLifeCycleIO m where
    liftLifeCycleIO :: forall a. LifeCycleIO a -> m a

instance {-# OVERLAPPING #-} MonadLifeCycleIO LifeCycleIO where
    liftLifeCycleIO lc = lc

instance (MonadTrans t, MonadIO (t m), MonadLifeCycleIO m) => MonadLifeCycleIO (t m) where
    liftLifeCycleIO lc = lift $ liftLifeCycleIO lc

instance (MonadTrans t, MonadTransConstraint MonadIO t) => MonadTransConstraint MonadLifeCycleIO t where
    hasTransConstraint ::
           forall m. MonadLifeCycleIO m
        => Dict (MonadLifeCycleIO (t m))
    hasTransConstraint =
        case hasTransConstraint @MonadIO @t @m of
            Dict -> Dict

class MonadLifeCycleIO m => MonadUnliftLifeCycleIO m where
    liftLifeCycleIOWithUnlift :: forall r. (MFunction m LifeCycleIO -> LifeCycleIO r) -> m r

instance MonadUnliftLifeCycleIO LifeCycleIO where
    liftLifeCycleIOWithUnlift call = call id
