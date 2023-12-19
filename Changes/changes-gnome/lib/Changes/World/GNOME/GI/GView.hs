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
    , gvGetUnliftToView
    , gvHoistView
    , gvSubLifecycle
    , gvLiftIO
    , gvLiftIONoUI
    , gvLiftLifecycle
    , gvGetLock
    , gvRunLockedState
    , gvRunLocked
    , gvRunUnlockedState
    , gvRunUnlocked
    , gvRunLocked'
    , gvRunLockedThen
    , gvSleep
    , gvExitUI
    , gvExitOnClosed
    , gvWithUnliftLockedAsync
    , gvNewEditSource
    , gvBindModelUpdates
    , gvBindModel
    , gvBindWholeModel
    , gvBindReadOnlyWholeModel
    , gvSetWholeModel
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

-- | Rules:
-- 1. Don't call GTK functions unless holding the GTK lock.
-- For this reason `liftIO` requires holding the lock.
--
-- 2. Don't aquire model "runner" locks while holding the GTK lock (to prevent deadlock).
-- Therefore, avoid calling code while holding the GTK lock unless you know it's safe.
--
-- 3. Call `gvRunUnlocked` only when necessary (generally when called by a signal handler).
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
       forall ls. Is LockStateType ls
    => GViewState ls
    -> GView ls ()
gvCloseState (MkGViewState NoLifeState) = return ()
gvCloseState state = MkGView $ liftIO $ closeLifeState $ gvsViewState state

gvOnClose ::
       forall ls. Is LockStateType ls
    => GView ls ()
    -> GView ls ()
gvOnClose gv = gvLiftViewWithUnliftNoUI $ \unlift -> viewOnClose $ unlift gv

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

gvLiftView :: View --> GView 'Unlocked
gvLiftView = MkGView . lift

gvLiftViewWithUnliftNoUI :: forall ls a. ((forall ls'. GView ls' --> View) -> View a) -> GView ls a
gvLiftViewWithUnliftNoUI call = MkGView $ liftWithUnlift $ \unlift -> call $ unlift . unGView

gvLiftViewWithUnlift :: forall a. ((GView 'Unlocked --> View) -> View a) -> GView 'Unlocked a
gvLiftViewWithUnlift call = gvLiftViewWithUnliftNoUI $ \unlift -> call unlift

gvLiftLifecycle :: Lifecycle --> GView 'Unlocked
gvLiftLifecycle = gvLiftView . viewLiftLifecycle

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

gvExitOnClosed :: GView 'Unlocked --> GView Unlocked
gvExitOnClosed gv = do
    gtkc <- gvGetContext
    gvHoistView (gtkcExitOnClosed gtkc) gv

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

gvRunUnlockedState :: GView ls (GViewState 'Unlocked -> GViewState 'Locked)
gvRunUnlockedState = do
    lock <- MkGView $ asks gtkcLock
    return $ \(MkGViewState state) -> MkGViewState $ viewStateRunUnlocked lock state

gvRunUnlocked :: GView 'Unlocked --> GView 'Locked
gvRunUnlocked (MkGView rva) =
    MkGView $ do
        lock <- asks gtkcLock
        hoist (viewCloseRunUnlocked lock . viewOpenRunUnlocked lock) rva

gvRunLocked' ::
       forall ls. Is LockStateType ls
    => GView 'Locked --> GView ls
gvRunLocked' =
    case lockStateType @ls of
        LockedType -> id
        UnlockedType -> gvRunLocked

gvRunLockedThen :: GView 'Locked (GView 'Unlocked a) -> GView 'Unlocked a
gvRunLockedThen mma = do
    ma <- gvRunLocked mma
    ma

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

gvWithUnliftLockedAsync ::
       forall ls a. Is LockStateType ls
    => ((GView 'Locked --> IO) -> GView ls a)
    -> GView ls a
gvWithUnliftLockedAsync call = do
    ctx <- gvGetContext
    gvLiftRelock @ls @ls $
        viewWithUnliftAsync $ \unlift -> gvUnliftRelock @ls @ls ctx $ call $ unlift . gvUnliftRelock @'Locked @ls ctx

gvNewEditSource :: GView ls EditSource
gvNewEditSource = gvLiftIONoUI newEditSource

gvBindModelUpdates ::
       forall update a.
       Model update
    -> (EditSource -> Bool)
    -> GView 'Unlocked a
    -> (a -> Task IO ())
    -> (a -> NonEmpty update -> EditContext -> GView 'Unlocked ())
    -> GView 'Unlocked a
gvBindModelUpdates model testesrc initv utask recv =
    gvLiftViewWithUnlift $ \unlift ->
        viewBindModelUpdates model testesrc (unlift initv) utask $ \a updates ec -> unlift $ recv a updates ec

gvBindModel ::
       forall update a.
       Model update
    -> Maybe EditSource
    -> GView 'Unlocked a
    -> (a -> Task IO ())
    -> (a -> NonEmpty update -> GView 'Unlocked ())
    -> GView 'Unlocked a
gvBindModel model mesrc initv utask recv =
    gvBindModelUpdates model (\ec -> mesrc /= Just ec) initv utask $ \a updates _ec -> recv a updates

gvBindWholeModel ::
       forall t. Model (WholeUpdate t) -> Maybe EditSource -> (t -> GView 'Unlocked ()) -> GView 'Unlocked ()
gvBindWholeModel model mesrc call =
    gvLiftViewWithUnlift $ \unlift -> viewBindWholeModel model mesrc $ \_finit t -> unlift $ call t

gvSetWholeModel :: forall t. Model (WholeUpdate t) -> EditSource -> t -> GView 'Unlocked Bool
gvSetWholeModel model esrc v =
    gvLiftView $ viewRunResource model $ \asub -> pushEdit esrc $ aModelEdit asub $ pure $ MkWholeReaderEdit v

gvBindReadOnlyWholeModel :: forall t. Model (ROWUpdate t) -> (t -> GView 'Unlocked ()) -> GView 'Unlocked ()
gvBindReadOnlyWholeModel model call =
    gvLiftViewWithUnlift $ \unlift -> viewBindReadOnlyWholeModel model $ \_finit t -> unlift $ call t

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

gvReplaceDynamicView :: GView 'Unlocked dvs -> (dvs -> GViewState 'Unlocked) -> StateT dvs (GView 'Unlocked) ()
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
            firstspec <- gvLiftView $ viewRunResource model $ \am -> aModelRead am ReadWhole
            ((), vs) <- gvGetState firstspec
            return (vs, ())
        recvVS :: () -> [ROWUpdate (GView 'Unlocked ())] -> StateT (GViewState 'Unlocked) (GView 'Unlocked) ()
        recvVS () updates =
            for_ (lastReadOnlyWholeUpdate updates) $ \spec -> gvReplaceDynamicView (getViewState spec) id
    gvDynamic model initVS return mempty recvVS

gvInnerWholeView ::
       forall f update. (MonadInner f, IsUpdate update, FullEdit (UpdateEdit update))
    => Model (FullResultOneUpdate f update)
    -> (f (Model update) -> GView 'Unlocked ())
    -> SelectNotify (f ())
    -> GView 'Unlocked ()
gvInnerWholeView model baseView seln =
    gvLiftViewWithUnlift $ \unlift -> viewInnerWholeView model (\fm -> unlift $ baseView fm) seln
