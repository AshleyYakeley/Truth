module Changes.World.GNOME.GI.LockState where

import Shapes

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
