module Changes.World.GNOME.GI.GView.View
    ( GTKContext (..)
    , GSemiview
    , GView
    , GViewState
    , gsvGetState
    , gvAddState
    , gsvCloseState
    , gvCloseState
    , gvOnClose
    , gvGetCloser
    , runGView
    , gvGetContext
    , gvLiftView
    , gvLiftViewWithUnlift
    , gvAskUnliftView
    , gvAskUnliftLifecycle
    , gvSubLifecycle
    , gvLiftIO
    , gvLiftIOTrustMeNoUI
    , gvLiftLifecycle
    , gvRunLocked
    , gvRunUnlocked
    , gvRunLocked'
    , gvRunLockedThen
    , gvExitUI
    , gvExitOnClosed
    , GTKAsyncUnlift
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
    , gvMkTask
    )
where

import Changes.World.GNOME.GI.GView.Context
import Changes.World.GNOME.GI.GView.Semiview
import Import

-- | Rules:
-- 1. Don't call GTK functions unless holding the GTK lock.
-- For this reason `liftIO` requires holding the lock.
--
-- 2. Don't acquire model "runner" locks while holding the GTK lock (to prevent deadlock).
-- Therefore, avoid calling code while holding the GTK lock unless you know it's safe.
--
-- 3. Call `gvRunUnlocked` only when necessary (generally when called by a signal handler).
type GView :: LockState -> Type -> Type
type GView ls = LifecycleT (GSemiview 'Unlocked) (GSemiview ls)

gvGetContext :: GView ls (GTKContext ls)
gvGetContext = lift gsvGetContext

type GViewState :: Type
newtype GViewState = MkGViewState
    { gvsViewState :: LifeState (GSemiview 'Unlocked)
    }
    deriving newtype (Semigroup, Monoid)

gsvGetState :: forall a. GView 'Unlocked a -> GSemiview 'Unlocked (a, GViewState)
gsvGetState gv = trustMeNoUI @'Unlocked $ do
    (a, state) <- getLifeState gv
    return (a, MkGViewState state)

gvAddState :: forall ls. GViewState -> GView ls ()
gvAddState state = trustMeNoUI @ls $ addLifeState $ gvsViewState state

gsvCloseState :: GViewState -> GSemiview 'Unlocked ()
gsvCloseState (MkGViewState NoLifeState) = return ()
gsvCloseState state = closeLifeState $ gvsViewState state

gvCloseState :: GViewState -> GView 'Unlocked ()
gvCloseState state = lift $ gsvCloseState state

gvOnClose :: forall lsa lsb. Is LockStateType lsa => GSemiview lsa () -> GView lsb ()
gvOnClose gv = trustMeNoUI @lsb $ lifecycleOnClose $ gsvRelock gv

gvGetCloser :: forall ls a. GView ls a -> GView ls (a, GSemiview 'Unlocked ())
gvGetCloser gv = trustMeNoUI @ls $ trustMeNoUI @'Unlocked $ lifecycleGetCloser gv

-- | `View` is implicitly unlocked.
runGView :: GTKContext 'Unlocked -> GView 'Unlocked --> View
runGView ctx = hoistLifecycleBoth $ runGSemiview ctx

gvLiftIO :: IO --> GView 'Locked
gvLiftIO = liftIO

gvLiftIOTrustMeNoUI :: forall ls. IO --> GView ls
gvLiftIOTrustMeNoUI = trustMeNoUI @ls liftIO

gvLiftViewAny :: View --> GView ls
gvLiftViewAny = hoist gsvLiftSemiviewAny . hoistLifecycleClose gsvLiftSemiviewAny

gvLiftView :: View --> GView 'Unlocked
gvLiftView = gvLiftViewAny

-- | Lift unlocked, because View may be run by any thread.
gvLiftViewWithSemiUnlift :: forall a. ((GSemiview 'Unlocked --> Semiview) -> View a) -> GView 'Unlocked a
gvLiftViewWithSemiUnlift call = tunnel
    $ \tun ->
        gsvLiftSemiviewWithUnlift $ \unlift ->
            tun $ hoistLifecycleClose gsvLiftSemiview $ call unlift

-- | Lift unlocked, because View may be run by any thread.
gvLiftViewWithUnlift :: forall a. ((GView 'Unlocked --> View) -> View a) -> GView 'Unlocked a
gvLiftViewWithUnlift call =
    gvLiftViewWithSemiUnlift
        $ \unlift ->
            call
                $ hoistLifecycleBoth unlift

gvLiftLifecycle :: Lifecycle --> GView 'Unlocked
gvLiftLifecycle = gvLiftView . viewLiftLifecycle

gvAskUnliftView :: forall ls. Is LockStateType ls => GView ls (WRaised (GView 'Unlocked) View)
gvAskUnliftView = do
    gtkc <- gvGetContext
    gtkcu <- trustMeNoUI @ls $ provideUnlocked gtkc
    return $ MkWRaised $ runGView gtkcu

gvAskUnliftLifecycle :: GView 'Unlocked (WRaised (GView 'Unlocked) Lifecycle)
gvAskUnliftLifecycle = do
    gv <- gvAskUnliftView
    vl <- gvLiftView viewAskUnliftLifecycle
    return $ vl . gv

gvExitUI :: GView ls ()
gvExitUI = do
    gtkc <- gvGetContext
    gvLiftIOTrustMeNoUI $ gtkcExit gtkc

gvExitOnClosed :: GView 'Unlocked ()
gvExitOnClosed = do
    gtkc <- gvGetContext
    gvLiftView $ gtkcExitOnClosed gtkc

gvSubLifecycle :: GView 'Unlocked --> GView 'Unlocked
gvSubLifecycle gva = trustMeNoUI @'Unlocked $ lift $ runLifecycle gva

gvRunLocked :: GView 'Locked --> GView 'Unlocked
gvRunLocked = hoist gsvRunLocked

gvRunUnlocked :: GView 'Unlocked --> GView 'Locked
gvRunUnlocked = hoist gsvRunUnlocked

gvRunLocked' ::
    forall ls.
    Is LockStateType ls =>
    GView 'Locked --> GView ls
gvRunLocked' =
    case lockStateType @ls of
        LockedType -> id
        UnlockedType -> gvRunLocked

gvRunLockedThen :: GView 'Locked (GView 'Unlocked a) -> GView 'Unlocked a
gvRunLockedThen mma = do
    ma <- gvRunLocked mma
    ma

type GTKAsyncUnlift def = GView 'Locked def -> IO def

gvUnliftRelock ::
    GTKContext 'Locked ->
    GView 'Locked --> View
gvUnliftRelock ctx = hoist (gsvUnliftRelock ctx) . hoistLifecycleClose (gsvUnliftRelock ctx)

gvWithUnliftLockedAsync ::
    forall def a.
    def ->
    (GTKAsyncUnlift def -> GView 'Locked a) ->
    GView 'Locked a
gvWithUnliftLockedAsync defaultVal call = do
    ctx <- gvGetContext
    MkWRaised unlift <- gvLiftViewAny viewAskUnliftIOAsync
    call
        $ handleExc (\ex -> gtkcThrow ctx ex >> return defaultVal)
        . unlift
        . gvUnliftRelock ctx

gvNewEditSource :: GView ls EditSource
gvNewEditSource = gvLiftIOTrustMeNoUI newEditSource

gvBindModelUpdates ::
    forall update a.
    Model update ->
    (EditSource -> Bool) ->
    GView 'Unlocked a ->
    (a -> Task IO ()) ->
    (a -> NonEmpty update -> EditContext -> GView 'Unlocked ()) ->
    GView 'Unlocked a
gvBindModelUpdates model testesrc initv utask recv =
    gvLiftViewWithUnlift $ \unlift ->
        viewBindModelUpdates model testesrc (unlift initv) utask $ \a updates ec -> unlift $ recv a updates ec

gvBindModel ::
    forall update a.
    Model update ->
    Maybe EditSource ->
    GView 'Unlocked a ->
    (a -> Task IO ()) ->
    (a -> NonEmpty update -> GView 'Unlocked ()) ->
    GView 'Unlocked a
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
    Model update ->
    GView 'Unlocked (dvs, a) ->
    (dvs -> Semiview GViewState) ->
    Task IO () ->
    (a -> [update] -> StateT dvs (GView 'Unlocked) ()) ->
    GView 'Unlocked a
gvDynamic model initCV tovsCV taskCV recvCV =
    gvLiftViewWithSemiUnlift $ \unlift ->
        viewDynamic model (hoistLifecycleBoth unlift initCV) (fmap (mapLifeState unlift . gvsViewState) . tovsCV) taskCV $ \a updates ->
            hoist (hoistLifecycleBoth unlift) $ recvCV a updates

gvReplaceDynamicView :: GView 'Unlocked dvs -> (dvs -> GViewState) -> StateT dvs (GView 'Unlocked) ()
gvReplaceDynamicView getNewDVS tovsCV = do
    olddvs <- get
    lift $ gvCloseState $ tovsCV olddvs
    newdvs <- lift getNewDVS
    put newdvs

gvSwitch :: Model (ROWUpdate (GView 'Unlocked ())) -> GView 'Unlocked ()
gvSwitch model = do
    let
        getViewState :: GView 'Unlocked () -> GView 'Unlocked GViewState
        getViewState gv = do
            ((), vs) <- lift $ gsvGetState gv
            return vs
        initVS :: GView 'Unlocked (GViewState, ())
        initVS = do
            firstspec <- gvLiftView $ viewRunResource model $ \am -> aModelRead am ReadWhole
            ((), vs) <- lift $ gsvGetState firstspec
            return (vs, ())
        recvVS :: () -> [ROWUpdate (GView 'Unlocked ())] -> StateT GViewState (GView 'Unlocked) ()
        recvVS () updates =
            for_ (lastReadOnlyWholeUpdate updates) $ \spec -> gvReplaceDynamicView (getViewState spec) id
    gvDynamic model initVS return mempty recvVS

gvInnerWholeView ::
    forall f update.
    MonadInner f =>
    Model (FullResultOneUpdate f update) ->
    (f (Model update) -> GView 'Unlocked ()) ->
    SelectNotify (f ()) ->
    GView 'Unlocked ()
gvInnerWholeView model baseView seln =
    gvLiftViewWithUnlift $ \unlift -> viewInnerWholeView model (\fm -> unlift $ baseView fm) seln

gvMkTask :: forall a. IO (a -> IO (), Task (GView 'Unlocked) a)
gvMkTask = do
    (report, task) <- mkTask
    return (report, hoistTask gvLiftIOTrustMeNoUI task)
