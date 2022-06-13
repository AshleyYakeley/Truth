module Changes.GI.GView
    ( LockedState(..)
    , GTKContext(..)
    , GView
    , GViewState
    , gvGetState
    , gvAddState
    , gvCloseState
    , gvsViewState
    , gvGetStateFromLocked
    , gvGetStateFromUnlocked
    , gvOnClose
    , gvGetCloser
    , runGView
    , gvLiftView
    , gvGetUnliftToView
    , gvHoistView
    , gvHoistViewNoUI
    , gvSubLifeCycle
    , gvLiftIONoUI
    , gvLiftViewNoUI
    , gvLiftLifeCycleNoUI
    , gvGetUnlift
    , gvRunLocked
    , gvRunUnlocked
    , gvExitUI
    , gvExitOnClosed
    , gvWithUnliftAsync
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
import Shapes

data GTKContext = MkGTKContext
    { gtkcLockVar :: MVar ()
    , gtkcExit :: IO ()
    , gtkcExitOnClosed :: View --> View
    }

data LockedState
    = Locked
    | Unlocked

type GView :: LockedState -> Type -> Type
newtype GView ls a = MkGView
    { unGView :: ReaderT GTKContext View a
    } deriving (Functor, Applicative, Monad, MonadFail, MonadFix, MonadException)

-- | Only run IO with the UI lock held.
deriving instance MonadIO (GView 'Locked)

deriving instance MonadHoistIO (GView 'Locked)

deriving instance MonadTunnelIO (GView 'Locked)

gvGetContext :: GView ls GTKContext
gvGetContext = MkGView ask

type GViewState :: LockedState -> Type
newtype GViewState ls =
    MkGViewState ViewState
    deriving (Semigroup, Monoid)

gvGetState :: GView ls a -> GView ls (a, GViewState ls)
gvGetState gv =
    gvLiftViewWithUnliftNoUI $ \unlift -> do
        (a, state) <- viewGetViewState $ unlift gv
        return (a, MkGViewState state)

gvAddState :: GViewState ls -> GView ls ()
gvAddState (MkGViewState state) = MkGView $ lift $ viewAddViewState state

gvCloseState :: GViewState ls -> GView ls ()
gvCloseState (MkGViewState state) = MkGView $ liftIO $ closeLifeState state

gvsViewState :: GViewState 'Unlocked -> ViewState
gvsViewState (MkGViewState state) = state

gvOnClose :: GView ls () -> GView ls ()
gvOnClose gv = gvLiftViewWithUnliftNoUI $ \unlift -> viewOnClose $ unlift gv

gvGetCloser :: forall ls a. GView ls a -> GView ls (a, GView ls ())
gvGetCloser gv =
    gvLiftViewWithUnliftNoUI $ \unlift -> do
        (a, closer) <- viewGetCloser $ unlift gv
        return (a, gvLiftIONoUI closer)

-- | `GView` is always run without holding the UI lock.
runGView :: GTKContext -> GView 'Unlocked --> View
runGView ctx (MkGView rva) = runReaderT rva ctx

gvLiftIONoUI :: IO --> GView ls
gvLiftIONoUI = MkGView . liftIO

gvLiftViewNoUI :: View --> GView ls
gvLiftViewNoUI = MkGView . lift

gvLiftView :: View --> GView 'Unlocked
gvLiftView = gvLiftViewNoUI

gvLiftViewWithUnliftNoUI :: forall ls a. ((forall ls'. GView ls' --> View) -> View a) -> GView ls a
gvLiftViewWithUnliftNoUI call = MkGView $ liftWithUnlift $ \unlift -> call $ unlift . unGView

gvHoistViewNoUI :: (View --> View) -> GView ls --> GView ls
gvHoistViewNoUI f gv = gvLiftViewWithUnliftNoUI $ \unlift -> f $ unlift gv

gvGetUnliftToView :: forall ls. GView ls (WMFunction (GView 'Unlocked) View)
gvGetUnliftToView = do
    gtkc <- gvGetContext
    return $ MkWMFunction $ runGView gtkc

gvHoistView :: (View --> View) -> GView 'Unlocked --> GView 'Unlocked
gvHoistView f gv = do
    MkWMFunction unlift <- gvGetUnliftToView
    gvLiftView $ f $ unlift gv

gvExitUI :: GView ls ()
gvExitUI = do
    gtkc <- gvGetContext
    gvLiftIONoUI $ gtkcExit gtkc

gvExitOnClosed :: GView ls --> GView ls
gvExitOnClosed gv = do
    gtkc <- gvGetContext
    gvHoistViewNoUI (gtkcExitOnClosed gtkc) gv

gvSubLifeCycle :: GView ls --> GView ls
gvSubLifeCycle (MkGView rva) = MkGView $ hoist viewSubLifeCycle rva

lockLifeState :: (IO () -> IO ()) -> LifeState -> LifeState
lockLifeState lock (MkLifeState closer) = MkLifeState $ lock closer

gvGetStateFromLocked :: GView ls (GViewState 'Locked -> GViewState 'Unlocked)
gvGetStateFromLocked = do
    lockVar <- MkGView $ asks gtkcLockVar
    return $ \(MkGViewState state) -> MkGViewState $ lockLifeState (mVarUnitRun lockVar) state

gvGetStateFromUnlocked :: GView ls (GViewState 'Unlocked -> GViewState 'Locked)
gvGetStateFromUnlocked = do
    lockVar <- MkGView $ asks gtkcLockVar
    return $ \(MkGViewState state) -> MkGViewState $ lockLifeState (mVarUnitUnlock lockVar) state

gvRunLocked :: GView 'Locked --> GView 'Unlocked
gvRunLocked gv = do
    lockVar <- MkGView $ asks gtkcLockVar
    (a, state) <- MkGView $ hoistIO (mVarUnitRun lockVar) $ unGView $ gvGetState gv
    mapstate <- gvGetStateFromLocked
    gvAddState $ mapstate state
    return a

gvRunUnlocked :: GView 'Unlocked --> GView 'Locked
gvRunUnlocked gv = do
    lockVar <- MkGView $ asks gtkcLockVar
    (a, state) <- MkGView $ hoistIO (mVarUnitUnlock lockVar) $ unGView $ gvGetState gv
    mapstate <- gvGetStateFromUnlocked
    gvAddState $ mapstate state
    return a

gvLiftLifeCycleNoUI :: LifeCycle --> GView ls
gvLiftLifeCycleNoUI = gvLiftViewNoUI . viewLiftLifeCycle

gvGetUnlift :: GView ls (WMFunction (GView 'Unlocked) IO)
gvGetUnlift = do
    unlift <- gvGetUnliftToView
    unliftView <- gvLiftViewNoUI viewUnliftView
    return $ unliftView . unlift

gvWithUnliftAsync :: forall ls a. ((GView 'Locked --> IO) -> GView ls a) -> GView ls a
gvWithUnliftAsync call =
    gvLiftViewWithUnliftNoUI $ \unliftG -> viewWithUnliftAsync $ \unlift -> unliftG $ call $ unlift . unliftG

gvBindModelUpdates ::
       forall ls update a.
       Model update
    -> (EditSource -> Bool)
    -> GView ls a
    -> (a -> Task ())
    -> (a -> NonEmpty update -> EditContext -> GView 'Unlocked ())
    -> GView ls a
gvBindModelUpdates model testesrc initv utask recv =
    gvLiftViewWithUnliftNoUI $ \unlift ->
        viewBindModelUpdates model testesrc (unlift initv) utask $ \a updates ec -> unlift $ recv a updates ec

gvBindModel ::
       forall ls update a.
       Model update
    -> Maybe EditSource
    -> GView ls a
    -> (a -> Task ())
    -> (a -> NonEmpty update -> GView 'Unlocked ())
    -> GView ls a
gvBindModel model mesrc initv utask recv =
    gvBindModelUpdates model (\ec -> mesrc /= Just ec) initv utask $ \a updates _ec -> recv a updates

gvBindWholeModel :: forall ls t. Model (WholeUpdate t) -> Maybe EditSource -> (t -> GView 'Unlocked ()) -> GView ls ()
gvBindWholeModel model mesrc call = do
    MkWMFunction unlift <- gvGetUnliftToView
    gvLiftViewNoUI $ viewBindWholeModel model mesrc $ unlift . call

gvBindReadOnlyWholeModel :: forall ls t. Model (ROWUpdate t) -> (t -> GView 'Unlocked ()) -> GView ls ()
gvBindReadOnlyWholeModel model call =
    gvLiftViewWithUnliftNoUI $ \unlift -> viewBindReadOnlyWholeModel model $ unlift . call

gvRunResource ::
       forall f r.
       Resource f
    -> (forall tt.
            (MonadTransStackUnlift tt, MonadUnliftIO (ApplyStack tt IO), MonadFail (ApplyStack tt IO)) =>
                    f tt -> ApplyStack tt IO r)
    -> GView 'Locked r
gvRunResource r call = MkGView $ lift $ viewRunResource r call

gvRunResourceContext ::
       forall ls f r.
       Resource f
    -> (forall tt.
            (MonadTransStackUnlift tt, MonadUnliftIO (ApplyStack tt View)) => StackUnliftAll tt -> f tt -> GView ls r)
    -> GView ls r
gvRunResourceContext r call =
    gvLiftViewWithUnliftNoUI $ \unlift -> viewRunResourceContext r $ \uu ftt -> unlift $ call uu ftt

gvDynamic ::
       forall ls dvs update a.
       Model update
    -> GView 'Unlocked (dvs, a)
    -> (dvs -> IO (GViewState 'Unlocked))
    -> Task ()
    -> (a -> [update] -> StateT dvs (GView 'Unlocked) ())
    -> GView ls a
gvDynamic model initCV tovsCV taskCV recvCV =
    gvLiftViewWithUnliftNoUI $ \unlift ->
        viewDynamic model (unlift initCV) (fmap gvsViewState . tovsCV) taskCV $ \a updates ->
            hoist unlift $ recvCV a updates

gvReplaceDynamicView :: GView ls dvs -> (dvs -> GViewState ls) -> StateT dvs (GView ls) ()
gvReplaceDynamicView getNewDVS tovsCV = do
    olddvs <- get
    lift $ gvCloseState $ tovsCV olddvs
    newdvs <- lift getNewDVS
    put newdvs

gvSwitch :: Model (ROWUpdate (GView 'Unlocked ())) -> GView ls ()
gvSwitch model = do
    let
        getViewState :: GView 'Unlocked () -> GView 'Unlocked (GViewState 'Unlocked)
        getViewState gv = do
            ((), vs) <- gvGetState gv
            return vs
        initVS :: GView 'Unlocked (GViewState 'Unlocked, ())
        initVS = do
            firstspec <- gvLiftViewNoUI $ viewRunResource model $ \am -> aModelRead am ReadWhole
            ((), vs) <- gvGetState firstspec
            return (vs, ())
        recvVS :: () -> [ROWUpdate (GView 'Unlocked ())] -> StateT (GViewState 'Unlocked) (GView 'Unlocked) ()
        recvVS () updates =
            for_ (lastReadOnlyWholeUpdate updates) $ \spec -> gvReplaceDynamicView (getViewState spec) id
    gvDynamic model initVS return mempty recvVS

gvInnerWholeView ::
       forall ls f update. (MonadInner f, IsUpdate update, FullEdit (UpdateEdit update))
    => Model (FullResultOneUpdate f update)
    -> (f (Model update) -> GView 'Unlocked ())
    -> SelectNotify (f ())
    -> GView ls ()
gvInnerWholeView model baseView seln =
    gvLiftViewWithUnliftNoUI $ \unlift -> viewInnerWholeView model (\fm -> unlift $ baseView fm) seln
