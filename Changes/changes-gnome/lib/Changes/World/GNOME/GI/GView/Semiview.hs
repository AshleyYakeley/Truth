{-# OPTIONS -Wno-redundant-constraints #-}

module Changes.World.GNOME.GI.GView.Semiview
    ( HasNoUnlockedUICalls
    , trustMeNoUI
    , GSemiview
    , runGSemiviewOnGTKThread
    , runGSemiview
    , gsvLiftIO
    , gsvLiftIOTrustMeNoUI
    , gsvRunLocked
    , gsvRunUnlocked
    , gsvRelock
    , gsvGetContext
    , gsvLiftSemiviewAny
    , gsvLiftSemiview
    , gsvLiftSemiviewWithUnlift
    , gsvLiftRelock
    , gsvUnliftRelock
    , gsvSleep
    )
where

import Changes.World.GNOME.GI.GView.Context
import Import

newtype GSemiview ls a = MkGSemiview {unGSemiview :: LockableT GTKContext ls Semiview a}
    deriving newtype (Functor, Applicative, Monad, MonadFail, MonadFix, MonadException)

data HasNoUnlockedUICalls (ls :: LockState) = MkHasNoUnlockedUICalls

-- | No unlocked anything when locked.
instance Given (HasNoUnlockedUICalls 'Locked) where
    given = MkHasNoUnlockedUICalls

trustMeNoUI :: forall (ls :: LockState) r. (Given (HasNoUnlockedUICalls ls) => r) -> r
trustMeNoUI = give MkHasNoUnlockedUICalls

-- | Only run IO when it doesn't include UI calls, while Unlocked.
deriving newtype instance Given (HasNoUnlockedUICalls ls) => MonadIO (GSemiview ls)

deriving newtype instance Given (HasNoUnlockedUICalls ls) => MonadHoistIO (GSemiview ls)

deriving newtype instance Given (HasNoUnlockedUICalls ls) => MonadTunnelIO (GSemiview ls)

instance Given (HasNoUnlockedUICalls ls) => MonadUnliftIO (GSemiview ls) where
    liftIOWithUnlift call = MkGSemiview $ liftIOWithUnlift $ \unlift -> call $ unlift . unGSemiview

-- | Run this only when you already know you're on the GTK thread.
runGSemiviewOnGTKThread :: GTKContext ls -> GSemiview ls --> Semiview
runGSemiviewOnGTKThread ctx (MkGSemiview ma) = runLockableT ctx ma

-- | A `Semiview` is implicitly unlocked.
runGSemiview :: GTKContext 'Unlocked -> GSemiview 'Unlocked --> Semiview
runGSemiview = runGSemiviewOnGTKThread

gsvLiftIO :: IO --> GSemiview 'Locked
gsvLiftIO = liftIO

gsvLiftIOTrustMeNoUI :: forall ls. IO --> GSemiview ls
gsvLiftIOTrustMeNoUI = trustMeNoUI @ls liftIO

gsvRunLocked :: GSemiview 'Locked --> GSemiview 'Unlocked
gsvRunLocked (MkGSemiview ma) = MkGSemiview $ lockableTRunLocked ma

gsvRunUnlocked :: GSemiview 'Unlocked --> GSemiview 'Locked
gsvRunUnlocked (MkGSemiview ma) = MkGSemiview $ lockableTRunUnlocked ma

gsvRelock ::
    forall lsfrom lsto.
    (Is LockStateType lsfrom, Is LockStateType lsto) =>
    GSemiview lsfrom --> GSemiview lsto
gsvRelock = case lockStateType @lsfrom of
    UnlockedType -> case lockStateType @lsto of
        UnlockedType -> id
        LockedType -> gsvRunUnlocked
    LockedType -> case lockStateType @lsto of
        UnlockedType -> gsvRunLocked
        LockedType -> id

gsvGetContext :: GSemiview ls (GTKContext ls)
gsvGetContext = MkGSemiview lockableTGetLock

gsvLiftSemiviewAny :: forall ls. Semiview --> GSemiview ls
gsvLiftSemiviewAny sva = MkGSemiview $ lift sva

-- | Lift unlocked, because Semiview may be run by any thread.
gsvLiftSemiview :: Semiview --> GSemiview 'Unlocked
gsvLiftSemiview = gsvLiftSemiviewAny

-- | Lift unlocked, because Semiview may be run by any thread.
gsvLiftSemiviewWithUnlift :: forall a. ((GSemiview 'Unlocked --> Semiview) -> Semiview a) -> GSemiview 'Unlocked a
gsvLiftSemiviewWithUnlift call = MkGSemiview $ liftWithUnlift $ \unlift -> call $ unlift . unGSemiview

gsvLiftRelock ::
    forall lsfrom lsto.
    (Is LockStateType lsfrom, Is LockStateType lsto) =>
    Semiview --> GSemiview lsto
gsvLiftRelock = gsvRelock @lsfrom @lsto . gsvLiftSemiviewAny @lsfrom

gsvUnliftRelock ::
    forall lsfrom lsto.
    (Is LockStateType lsfrom, Is LockStateType lsto) =>
    GTKContext lsto -> GSemiview lsfrom --> Semiview
gsvUnliftRelock ctx = runGSemiviewOnGTKThread ctx . gsvRelock @lsfrom @lsto

gsvSleep :: Int -> GSemiview ls ()
gsvSleep mus = gsvLiftIOTrustMeNoUI $ threadDelay mus
