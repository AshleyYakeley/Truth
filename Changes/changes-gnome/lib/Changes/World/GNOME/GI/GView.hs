module Changes.World.GNOME.GI.GView
    ( GTKContext(..)
    , GView
    , GViewState
    , gvGetState
    , gvAddState
    , gvCloseState
    , gvOnClose
    , gvGetCloser
    , runGView
    , gvGetContext
    , gvLiftView
    , gvLiftViewWithUnlift
    , gvLiftRelock
    , gvUnliftRelock
    , gvGetUnliftToView
    , gvHoistView
    , gvHoistViewNoUI
    , gvSubLifecycle
    , gvLiftIO
    , gvLiftIONoUI
    , gvLiftViewNoUI
    , gvLiftLifecycleNoUI
    , gvGetLock
    , gvRunLockedState
    , gvRunLocked
    , gvRunLockedIO
    , gvRunUnlockedState
    , gvRunUnlocked
    , gvRunUnlockedIO
    , gvSleep
    , gvExitUI
    , gvExitOnClosed
    , gvWithUnliftLockedAsync
    , gvRunResource
    , gvRunResourceContext
    , gvBindModelUpdates
    , gvBindModel
    , gvBindWholeModel
    , gvBindReadOnlyWholeModel
    , gvDynamic
    , gvReplaceDynamicView
    , gvSwitch
    , gvInnerWholeView
    ) where

import Changes.Core
import Changes.World.GNOME.GI.LockState
import Changes.World.GNOME.GI.ViewLock
import Shapes

data GTKContext = MkGTKContext
    { gtkcLock :: CallbackLock
    , gtkcExit :: IO ()
    , gtkcExitOnClosed :: View --> View
    }

type GView :: LockState -> Type -> Type
newtype GView ls a = MkGView
    { unGView :: ReaderT GTKContext View a
    } deriving (Functor, Applicative, Monad, MonadFail, MonadFix, MonadException)

-- | Only run IO with the UI lock held.
deriving instance MonadIO (GView 'Locked)

deriving instance MonadHoistIO (GView 'Locked)

deriving instance MonadTunnelIO (GView 'Locked)

instance MonadUnliftIO (GView 'Locked) where
    liftIOWithUnlift call = MkGView $ liftIOWithUnlift $ \unlift -> call $ unlift . unGView

gvGetContext :: GView ls GTKContext
gvGetContext = MkGView ask

type GViewState :: LockState -> Type
newtype GViewState ls = MkGViewState
    { gvsViewState :: ViewState
    } deriving (Semigroup, Monoid)

gvGetState :: GView ls a -> GView ls (a, GViewState ls)
gvGetState gv =
    gvLiftViewWithUnliftNoUI $ \unlift -> do
        (a, state) <- viewGetViewState $ unlift gv
        return (a, MkGViewState state)

gvAddState :: GViewState ls -> GView ls ()
gvAddState state = MkGView $ lift $ viewAddViewState $ gvsViewState state

gvCloseState ::
       forall lsa lsb. (Is LockStateType lsa, Is LockStateType lsb)
    => GViewState lsa
    -> GView lsb ()
gvCloseState (MkGViewState NoLifeState) = return ()
gvCloseState state = gvMatchLock @lsa @lsb $ MkGView $ liftIO $ closeLifeState $ gvsViewState state

gvOnClose ::
       forall lsa lsb. (Is LockStateType lsa, Is LockStateType lsb)
    => GView lsa ()
    -> GView lsb ()
gvOnClose gv = gvLiftViewWithUnliftNoUI $ \unlift -> viewOnClose $ unlift $ gvMatchLock @lsa @lsb gv

gvGetCloser :: forall ls a. GView ls a -> GView ls (a, GView ls ())
gvGetCloser gv =
    gvLiftViewWithUnliftNoUI $ \unlift -> do
        (a, closer) <- viewGetCloser $ unlift gv
        return (a, gvLiftIONoUI closer)

-- | `GView` is always run without holding the UI lock.
runGView :: GTKContext -> GView 'Unlocked --> View
runGView ctx (MkGView rva) = runReaderT rva ctx

gvLiftIO :: IO --> GView 'Locked
gvLiftIO = liftIO

gvLiftIONoUI :: IO --> GView ls
gvLiftIONoUI = MkGView . liftIO

gvLiftViewNoUI :: View --> GView ls
gvLiftViewNoUI = MkGView . lift

gvLiftView :: View --> GView 'Unlocked
gvLiftView = gvLiftViewNoUI

gvLiftViewWithUnliftNoUI :: forall ls a. ((forall ls'. GView ls' --> View) -> View a) -> GView ls a
gvLiftViewWithUnliftNoUI call = MkGView $ liftWithUnlift $ \unlift -> call $ unlift . unGView

gvLiftViewWithUnlift :: forall a. ((GView 'Unlocked --> View) -> View a) -> GView 'Unlocked a
gvLiftViewWithUnlift call = gvLiftViewWithUnliftNoUI $ \unlift -> call unlift

gvHoistViewNoUI :: (View --> View) -> GView ls --> GView ls
gvHoistViewNoUI f gv = gvLiftViewWithUnliftNoUI $ \unlift -> f $ unlift gv

gvGetUnliftToView :: forall ls. GView ls (WRaised (GView 'Unlocked) View)
gvGetUnliftToView = do
    gtkc <- gvGetContext
    return $ MkWRaised $ runGView gtkc

gvHoistView :: (View a -> View b) -> GView ls a -> GView ls b
gvHoistView f (MkGView gv) = MkGView $ monoHoist f gv

gvSleep :: Int -> GView 'Unlocked ()
gvSleep mus = gvLiftIONoUI $ threadDelay mus

gvExitUI :: GView ls ()
gvExitUI = do
    gtkc <- gvGetContext
    gvLiftIONoUI $ gtkcExit gtkc

gvExitOnClosed :: GView ls --> GView ls
gvExitOnClosed gv = do
    gtkc <- gvGetContext
    gvHoistViewNoUI (gtkcExitOnClosed gtkc) gv

gvSubLifecycle :: GView ls --> GView ls
gvSubLifecycle (MkGView rva) = MkGView $ hoist viewSubLifecycle rva

gvGetLock :: GView ls CallbackLock
gvGetLock = MkGView $ asks gtkcLock

gvRunLockedState :: GView ls (GViewState 'Locked -> GViewState 'Unlocked)
gvRunLockedState = do
    lock <- MkGView $ asks gtkcLock
    return $ \(MkGViewState state) -> MkGViewState $ viewStateRunLocked lock state

gvRunLocked :: GView 'Locked --> GView 'Unlocked
gvRunLocked (MkGView rva) =
    MkGView $ do
        lock <- asks gtkcLock
        hoist (viewCloseRunLocked lock . viewOpenRunLocked lock) rva

gvRunLockedIO :: IO --> GView 'Unlocked
gvRunLockedIO ioa =
    MkGView $ do
        lock <- asks gtkcLock
        liftIO $ cbRunLocked lock ioa

gvRunUnlockedState :: GView ls (GViewState 'Unlocked -> GViewState 'Locked)
gvRunUnlockedState = do
    lock <- MkGView $ asks gtkcLock
    return $ \(MkGViewState state) -> MkGViewState $ viewStateRunUnlocked lock state

gvRunUnlocked :: GView 'Unlocked --> GView 'Locked
gvRunUnlocked (MkGView rva) =
    MkGView $ do
        lock <- asks gtkcLock
        hoist (viewCloseRunUnlocked lock . viewOpenRunUnlocked lock) rva

gvRunUnlockedIO :: IO --> GView 'Locked
gvRunUnlockedIO ioa =
    MkGView $ do
        lock <- asks gtkcLock
        liftIO $ cbRunUnlocked lock ioa

gvMatchLock ::
       forall lsa lsb. (Is LockStateType lsa, Is LockStateType lsb)
    => GView lsa --> GView lsb
gvMatchLock =
    case (lockStateType @lsa, lockStateType @lsb) of
        (LockedType, LockedType) -> id
        (UnlockedType, UnlockedType) -> id
        (LockedType, UnlockedType) -> gvRunLocked
        (UnlockedType, LockedType) -> gvRunUnlocked

gvUnliftRelock ::
       forall lsopen lsclose ls. (Is LockStateType lsopen, Is LockStateType lsclose, Is LockStateType ls)
    => GTKContext
    -> GView ls --> View
gvUnliftRelock ctx (MkGView rva) = let
    lock = gtkcLock ctx
    in viewRelockClose @ls @lsclose lock $ viewRelockOpen @ls @lsopen lock $ runReaderT rva ctx

gvLiftRelock ::
       forall lsopen lsclose ls. (Is LockStateType lsopen, Is LockStateType lsclose, Is LockStateType ls)
    => View --> GView ls
gvLiftRelock va =
    MkGView $ do
        lock <- asks gtkcLock
        lift $ viewRelockClose @lsclose @ls lock $ viewRelockOpen @lsopen @ls lock va

gvLiftLifecycleNoUI :: Lifecycle --> GView ls
gvLiftLifecycleNoUI = gvLiftViewNoUI . viewLiftLifecycle

gvWithUnliftLockedAsync ::
       forall ls a. Is LockStateType ls
    => ((GView 'Locked --> IO) -> GView ls a)
    -> GView ls a
gvWithUnliftLockedAsync call = do
    ctx <- gvGetContext
    gvLiftRelock @ls @ls $
        viewWithUnliftAsync $ \unlift -> gvUnliftRelock @ls @ls ctx $ call $ unlift . gvUnliftRelock @'Locked @ls ctx

gvBindModelUpdates ::
       forall ls update a. Is LockStateType ls
    => Model update
    -> (EditSource -> Bool)
    -> GView ls a
    -> (a -> Task IO ())
    -> (a -> NonEmpty update -> EditContext -> GView 'Unlocked ())
    -> GView ls a
gvBindModelUpdates model testesrc initv utask recv = do
    ctx <- gvGetContext
    gvLiftRelock @ls @ls $
        viewBindModelUpdates model testesrc (gvUnliftRelock @ls @ls ctx initv) utask $ \a updates ec ->
            gvUnliftRelock @'Unlocked @ls ctx $ recv a updates ec

gvBindModel ::
       forall ls update a. Is LockStateType ls
    => Model update
    -> Maybe EditSource
    -> GView ls a
    -> (a -> Task IO ())
    -> (a -> NonEmpty update -> GView 'Unlocked ())
    -> GView ls a
gvBindModel model mesrc initv utask recv =
    gvBindModelUpdates model (\ec -> mesrc /= Just ec) initv utask $ \a updates _ec -> recv a updates

gvBindWholeModel ::
       forall ls lsr t. (Is LockStateType ls, Is LockStateType lsr)
    => Model (WholeUpdate t)
    -> Maybe EditSource
    -> (t -> GView lsr ())
    -> GView ls ()
gvBindWholeModel model mesrc call = do
    ctx <- gvGetContext
    gvLiftRelock @ls @ls $
        viewBindWholeModel model mesrc $ \finit t ->
            if finit
                then gvUnliftRelock @ls @ls ctx $ call t
                else gvUnliftRelock @'Unlocked @ls ctx $ call t

gvBindReadOnlyWholeModel ::
       forall ls lsr t. (Is LockStateType ls, Is LockStateType lsr)
    => Model (ROWUpdate t)
    -> (t -> GView lsr ())
    -> GView ls ()
gvBindReadOnlyWholeModel model call = do
    ctx <- gvGetContext
    gvLiftRelock @ls @ls $
        viewBindReadOnlyWholeModel model $ \finit t ->
            if finit
                then gvUnliftRelock @ls @ls ctx $ call t
                else gvUnliftRelock @'Unlocked @ls ctx $ call t

gvRunResource ::
       forall ls f r.
       Resource f
    -> (forall tt.
            (MonadTransStackUnlift tt, MonadUnliftIO (ApplyStack tt IO), MonadFail (ApplyStack tt IO)) =>
                    f tt -> ApplyStack tt IO r)
    -> GView ls r
gvRunResource r call = MkGView $ lift $ viewRunResource r call

gvRunResourceContext ::
       forall ls f r.
       Resource f
    -> (forall tt.
            (MonadTransStackUnlift tt, MonadUnliftIO (ApplyStack tt View)) => StackUnlift tt -> f tt -> GView ls r)
    -> GView ls r
gvRunResourceContext r call =
    gvLiftViewWithUnliftNoUI $ \unlift -> viewRunResourceContext r $ \uu ftt -> unlift $ call uu ftt

gvDynamic ::
       forall dvs update a.
       Model update
    -> GView 'Unlocked (dvs, a)
    -> (dvs -> IO (GViewState 'Unlocked))
    -> Task IO ()
    -> (a -> [update] -> StateT dvs (GView 'Unlocked) ())
    -> GView 'Unlocked a
gvDynamic model initCV tovsCV taskCV recvCV =
    gvLiftViewWithUnlift $ \unlift ->
        viewDynamic model (unlift initCV) (fmap gvsViewState . tovsCV) taskCV $ \a updates ->
            hoist unlift $ recvCV a updates

gvReplaceDynamicView :: Is LockStateType ls => GView ls dvs -> (dvs -> GViewState 'Unlocked) -> StateT dvs (GView ls) ()
gvReplaceDynamicView getNewDVS tovsCV = do
    olddvs <- get
    lift $ gvCloseState $ tovsCV olddvs
    newdvs <- lift getNewDVS
    put newdvs

gvSwitch :: Model (ROWUpdate (GView 'Unlocked ())) -> GView 'Unlocked ()
gvSwitch model = do
    let
        getViewState :: GView 'Unlocked () -> GView 'Unlocked (GViewState 'Unlocked)
        getViewState gv = do
            ((), vs) <- gvGetState gv
            return vs
        initVS :: GView 'Unlocked (GViewState 'Unlocked, ())
        initVS = do
            firstspec <- gvLiftViewNoUI $ viewRunResource model $ \am -> aModelRead am ReadWhole
            ((), vs) <- gvMatchLock $ gvGetState firstspec
            return (vs, ())
        recvVS :: () -> [ROWUpdate (GView 'Unlocked ())] -> StateT (GViewState 'Unlocked) (GView 'Unlocked) ()
        recvVS () updates =
            for_ (lastReadOnlyWholeUpdate updates) $ \spec -> gvReplaceDynamicView (getViewState spec) id
    gvDynamic model initVS return mempty recvVS

gvInnerWholeView ::
       forall ls f update. (Is LockStateType ls, MonadInner f, IsUpdate update, FullEdit (UpdateEdit update))
    => Model (FullResultOneUpdate f update)
    -> (f (Model update) -> GView 'Unlocked ())
    -> SelectNotify (f ())
    -> GView ls ()
gvInnerWholeView model baseView seln = do
    ctx <- gvGetContext
    gvLiftRelock @ls @ls $ viewInnerWholeView model (\fm -> gvUnliftRelock @ls @ls ctx $ baseView fm) seln
