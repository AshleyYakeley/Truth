module Changes.GI.ViewLock where

import Changes.Core
import Changes.Debug
import Changes.GI.LockState
import Shapes

viewOpenRunLocked :: CallbackLock -> View --> View
viewOpenRunLocked lock = hoistIO $ traceBarrier "open-runLocked" $ cbRunLocked lock

viewOpenRunUnlocked :: CallbackLock -> View --> View
viewOpenRunUnlocked lock = hoistIO $ traceBarrier "open-runUnlocked" $ cbRunUnlocked lock

viewStateRunLocked :: CallbackLock -> ViewState -> ViewState
viewStateRunLocked _ (MkLifeState Nothing) = MkLifeState Nothing
viewStateRunLocked lock (MkLifeState (Just lfs)) =
    MkLifeState $ Just $ traceBarrier "close-runLocked" (cbRunLocked lock) lfs

viewStateRunUnlocked :: CallbackLock -> ViewState -> ViewState
viewStateRunUnlocked _ (MkLifeState Nothing) = MkLifeState Nothing
viewStateRunUnlocked lock (MkLifeState (Just lfs)) =
    MkLifeState $ Just $ traceBarrier "close-runUnlocked" (cbRunUnlocked lock) lfs

viewCloseRunLocked :: CallbackLock -> View --> View
viewCloseRunLocked lock va = do
    (a, state) <- viewGetViewState va
    viewAddViewState $ viewStateRunLocked lock state
    return a

viewCloseRunUnlocked :: CallbackLock -> View --> View
viewCloseRunUnlocked lock =
    traceBarrier "viewCloseRunUnlocked" $ \va -> do
        (a, state) <- traceBarrier "viewCloseRunUnlocked.GVS" viewGetViewState va
        traceBracket "viewCloseRunUnlocked.AVS" $ viewAddViewState $ viewStateRunUnlocked lock state
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
