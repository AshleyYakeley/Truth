module Control.Lock.IsLock
    ( LockState (..)
    , LockStateType (..)
    , lockStateType
    , IsLock (..)
    , runLocked
    , runUnlocked
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

class IsLock (lock :: Type) where
    runLockedIO :: lock -> IO --> IO
    runUnlockedIO :: lock -> IO --> IO

runLocked :: forall lock m. (IsLock lock, MonadHoistIO m) => lock -> m --> m
runLocked lock = hoistIO $ runLockedIO lock

runUnlocked :: forall lock m. (IsLock lock, MonadHoistIO m) => lock -> m --> m
runUnlocked lock = hoistIO $ runUnlockedIO lock

instance IsLock () where
    runLockedIO () = id
    runUnlockedIO () = id

instance (IsLock p, IsLock q) => IsLock (p, q) where
    runLockedIO (pl, ql) = runLockedIO pl . runLockedIO ql
    runUnlockedIO (pl, ql) = runUnlockedIO ql . runUnlockedIO pl
