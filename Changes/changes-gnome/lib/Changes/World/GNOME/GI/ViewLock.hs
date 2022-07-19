module Changes.World.GNOME.GI.ViewLock where

import Changes.Core
import Changes.World.GNOME.GI.LockState
import Shapes

viewOpenRunLocked :: CallbackLock -> View --> View
viewOpenRunLocked lock = hoistIO $ cbRunLocked lock

viewOpenRunUnlocked :: CallbackLock -> View --> View
viewOpenRunUnlocked lock = hoistIO $ cbRunUnlocked lock

viewStateRunLocked :: CallbackLock -> ViewState -> ViewState
viewStateRunLocked _ (MkLifeState Nothing) = MkLifeState Nothing
viewStateRunLocked lock (MkLifeState (Just lfs)) = MkLifeState $ Just $ cbRunLocked lock lfs

viewStateRunUnlocked :: CallbackLock -> ViewState -> ViewState
viewStateRunUnlocked _ (MkLifeState Nothing) = MkLifeState Nothing
viewStateRunUnlocked lock (MkLifeState (Just lfs)) = MkLifeState $ Just $ cbRunUnlocked lock lfs

viewCloseRunLocked :: CallbackLock -> View --> View
viewCloseRunLocked lock va = do
    (a, state) <- viewGetViewState va
    viewAddViewState $ viewStateRunLocked lock state
    return a

viewCloseRunUnlocked :: CallbackLock -> View --> View
viewCloseRunUnlocked lock va = do
    (a, state) <- viewGetViewState va
    viewAddViewState $ viewStateRunUnlocked lock state
    return a

viewRelockOpen ::
       forall lsa lsb. (Is LockStateType lsa, Is LockStateType lsb)
    => CallbackLock
    -> View --> View
viewRelockOpen lock =
    case (lockStateType @lsa, lockStateType @lsb) of
        (LockedType, LockedType) -> id
        (UnlockedType, UnlockedType) -> id
        (LockedType, UnlockedType) -> viewOpenRunLocked lock
        (UnlockedType, LockedType) -> viewOpenRunUnlocked lock

viewRelockClose ::
       forall lsa lsb. (Is LockStateType lsa, Is LockStateType lsb)
    => CallbackLock
    -> View --> View
viewRelockClose lock =
    case (lockStateType @lsa, lockStateType @lsb) of
        (LockedType, LockedType) -> id
        (UnlockedType, UnlockedType) -> id
        (LockedType, UnlockedType) -> viewCloseRunLocked lock
        (UnlockedType, LockedType) -> viewCloseRunUnlocked lock
