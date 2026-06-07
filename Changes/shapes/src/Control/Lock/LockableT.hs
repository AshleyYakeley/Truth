module Control.Lock.LockableT
    ( LockableT
    , runLockableT
    , mkLockableT
    , lockableTGetLock
    , lockableTRunLocked
    , lockableTRunUnlocked
    )
where

import Control.Lock.IsLock
import Shapes.Import

newtype LockableT (lock :: Type) (ls :: LockState) (m :: Type -> Type) (a :: Type)
    = MkLockableT {unLockableT :: ReaderT lock m a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadFail
        , MonadFix
        , MonadException
        , MonadIO
        , MonadTrans
        , MonadTransHoist
        , MonadTransTunnel
        )

instance TransConstraint Monad (LockableT lock ls) where
    hasTransConstraint = Dict

instance TransConstraint MonadFix (LockableT lock ls) where
    hasTransConstraint = Dict

instance TransConstraint MonadIO (LockableT lock ls) where
    hasTransConstraint = Dict

instance TransConstraint MonadFail (LockableT lock ls) where
    hasTransConstraint = Dict

instance MonadTransUnlift (LockableT lock ls) where
    liftWithUnlift call = MkLockableT $ liftWithUnlift $ \unlift -> call $ unlift . unLockableT

runLockableT :: lock -> LockableT lock ls m a -> m a
runLockableT lock (MkLockableT rma) = runReaderT rma lock

mkLockableT :: (lock -> m a) -> LockableT lock ls m a
mkLockableT rma = MkLockableT $ ReaderT rma

lockableTGetLock :: forall lock ls m. Monad m => LockableT lock ls m lock
lockableTGetLock = MkLockableT ask

lockableTRunLocked :: forall lock m. (IsLock lock, MonadTunnelIO m) => LockableT lock 'Locked m --> LockableT lock 'Unlocked m
lockableTRunLocked (MkLockableT (ReaderT rma)) = MkLockableT $ ReaderT $ \lock -> runLocked lock $ rma lock

lockableTRunUnlocked :: forall lock m. (IsLock lock, MonadTunnelIO m) => LockableT lock 'Unlocked m --> LockableT lock 'Locked m
lockableTRunUnlocked (MkLockableT (ReaderT rma)) =
    MkLockableT $ ReaderT $ \lock -> runUnlocked lock $ rma lock
