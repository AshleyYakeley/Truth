module Control.Lock.IsLock
    ( LockState (..)
    , LockStateType (..)
    , lockStateType
    , IsLock (..)
    , runLocked
    , runUnlocked
    , provideUnlocked
    )
where

import Shapes.Import

data LockState
    = Locked
    | Unlocked

type LockStateType :: LockState -> Type
data LockStateType t where
    LockedType :: LockStateType 'Locked
    UnlockedType :: LockStateType 'Unlocked

instance Show (LockStateType ls) where
    show LockedType = "locked"
    show UnlockedType = "unlocked"

instance TestEquality LockStateType where
    testEquality LockedType LockedType = Just Refl
    testEquality UnlockedType UnlockedType = Just Refl
    testEquality _ _ = Nothing

instance Representative LockStateType where
    getRepWitness LockedType = Dict
    getRepWitness UnlockedType = Dict

instance Is LockStateType 'Locked where
    representative = LockedType

instance Is LockStateType 'Unlocked where
    representative = UnlockedType

lockStateType ::
    forall (ls :: LockState).
    Is LockStateType ls =>
    LockStateType ls
lockStateType = representative @_ @_ @ls

class IsLock (lock :: LockState -> Type) where
    runLockedIO :: forall a. lock 'Unlocked -> (lock 'Locked -> IO a) -> IO a
    runUnlockedIO :: forall a. lock 'Locked -> (lock 'Unlocked -> IO a) -> IO a

    -- | for providing to another thread
    provideUnlockedIO :: lock 'Locked -> IO (lock 'Unlocked)

runLocked :: forall lock m a. (IsLock lock, MonadTunnelIO m) => lock 'Unlocked -> (lock 'Locked -> m a) -> m a
runLocked lock rma = tunnelIO $ \tun ->
    runLockedIO lock $ \lock' ->
        tun $ rma lock'

runUnlocked :: forall lock m a. (IsLock lock, MonadTunnelIO m) => lock 'Locked -> (lock 'Unlocked -> m a) -> m a
runUnlocked lock rma = tunnelIO $ \tun ->
    runUnlockedIO lock $ \lock' ->
        tun $ rma lock'

provideUnlocked :: forall lock ls m. (IsLock lock, Is LockStateType ls, MonadIO m) => lock ls -> m (lock 'Unlocked)
provideUnlocked lock = case lockStateType @ls of
    LockedType -> liftIO $ provideUnlockedIO lock
    UnlockedType -> return lock

instance IsLock (Const a) where
    runLockedIO (Const a) call = call $ Const a
    runUnlockedIO (Const a) call = call $ Const a
    provideUnlockedIO (Const a) = return $ Const a

instance (IsLock p, IsLock q) => IsLock (PairType p q) where
    runLockedIO (MkPairType pl ql) call =
        runLockedIO pl $ \pl' ->
            runLockedIO ql $ \ql' ->
                call $ MkPairType pl' ql'
    runUnlockedIO (MkPairType pl ql) call =
        runUnlockedIO ql $ \ql' ->
            runUnlockedIO pl $ \pl' ->
                call $ MkPairType pl' ql'
    provideUnlockedIO (MkPairType pl ql) = do
        pl' <- provideUnlockedIO pl
        ql' <- provideUnlockedIO ql
        return $ MkPairType pl' ql'
