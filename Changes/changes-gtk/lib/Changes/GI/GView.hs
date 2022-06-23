module Changes.GI.GView
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
    , gvHoistViewNoUI
    , gvSubLifeCycle
    , gvLiftIO
    , gvLiftIONoUI
    , gvLiftViewNoUI
    , gvLiftLifeCycleNoUI
    , gvGetUnlift
    , gvRunLocked
    , gvRunUnlocked
    , gvSleep
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
import Changes.GI.LockState
import Shapes

data GTKContext = MkGTKContext
    { gtkcLock :: CallbackLock
    , gtkcExit :: IO ()
    , gtkcExitOnClosed :: View --> View
    }

gvMatchLock ::
       forall lsa lsb. (Is LockStateType lsa, Is LockStateType lsb)
    => GView lsa --> GView lsb
gvMatchLock =
    case (lockStateType @lsa, lockStateType @lsb) of
        (LockedType, LockedType) -> id
        (UnlockedType, UnlockedType) -> id
        (LockedType, UnlockedType) -> gvRunLocked
        (UnlockedType, LockedType) -> gvRunUnlocked

type GView :: LockState -> Type -> Type
newtype GView ls a = MkGView
    { unGView :: ReaderT GTKContext View a
    } deriving (Functor, Applicative, Monad, MonadFail, MonadFix, MonadException)

-- | Only run IO with the UI lock held.
deriving instance MonadIO (GView ls)

deriving instance MonadHoistIO (GView 'Locked)

deriving instance MonadTunnelIO (GView 'Locked)

instance MonadUnliftIO (GView 'Locked) where
    liftIOWithUnlift call = MkGView $ liftIOWithUnlift $ \unlift -> call $ unlift . unGView

gvGetContext :: GView ls GTKContext
gvGetContext = MkGView ask

newtype GViewState = MkGViewState
    { gvsViewState :: ViewState
    } deriving (Semigroup, Monoid)

gvGetState :: GView ls a -> GView ls (a, GViewState)
gvGetState gv =
    gvLiftViewWithUnliftNoUI $ \unlift -> do
        (a, state) <- viewGetViewState $ unlift gv
        return (a, MkGViewState state)

gvAddState :: GViewState -> GView ls ()
gvAddState state = MkGView $ lift $ viewAddViewState $ gvsViewState state

gvCloseState :: Is LockStateType ls => GViewState -> GView ls ()
gvCloseState state = gvMatchLock @'Unlocked $ MkGView $ liftIO $ closeLifeState $ gvsViewState state

gvOnClose :: Is LockStateType lsa => GView lsa () -> GView lsb ()
gvOnClose gv = gvLiftViewWithUnliftNoUI $ \unlift -> viewOnClose $ unlift $ gvMatchLock @_ @'Unlocked gv

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

gvGetUnliftToView :: forall ls. GView ls (WMFunction (GView 'Unlocked) View)
gvGetUnliftToView = do
    gtkc <- gvGetContext
    return $ MkWMFunction $ runGView gtkc

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

gvSubLifeCycle :: GView ls --> GView ls
gvSubLifeCycle (MkGView rva) = MkGView $ hoist viewSubLifeCycle rva

gvRunLocked :: GView 'Locked --> GView 'Unlocked
gvRunLocked gv =
    MkGView $ do
        lock <- asks gtkcLock
        hoistIO (cbRunLocked lock) $ unGView gv

gvRunUnlocked :: GView 'Unlocked --> GView 'Locked
gvRunUnlocked gv =
    MkGView $ do
        lock <- asks gtkcLock
        hoistIO (cbRunUnlocked lock) $ unGView gv

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

gvBindReadOnlyWholeModel ::
       forall ls lsr t. (Is LockStateType ls, Is LockStateType lsr)
    => Model (ROWUpdate t)
    -> (t -> GView lsr ())
    -> GView ls ()
gvBindReadOnlyWholeModel model call =
    gvLiftViewWithUnliftNoUI $ \unlift ->
        viewBindReadOnlyWholeModel model $ \init t ->
            if init
                then unlift $ gvMatchLock @lsr @ls $ call t
                else unlift $ gvMatchLock @lsr @'Unlocked $ call t

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
       forall dvs update a.
       Model update
    -> GView 'Unlocked (dvs, a)
    -> (dvs -> IO GViewState)
    -> Task ()
    -> (a -> [update] -> StateT dvs (GView 'Unlocked) ())
    -> GView 'Unlocked a
gvDynamic model initCV tovsCV taskCV recvCV =
    gvLiftViewWithUnlift $ \unlift ->
        viewDynamic model (unlift initCV) (fmap gvsViewState . tovsCV) taskCV $ \a updates ->
            hoist unlift $ recvCV a updates

gvReplaceDynamicView :: Is LockStateType ls => GView ls dvs -> (dvs -> GViewState) -> StateT dvs (GView ls) ()
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
            ((), vs) <- gvGetState gv
            return vs
        initVS :: GView 'Unlocked (GViewState, ())
        initVS = do
            firstspec <- gvLiftViewNoUI $ viewRunResource model $ \am -> aModelRead am ReadWhole
            ((), vs) <- gvMatchLock $ gvGetState firstspec
            return (vs, ())
        recvVS :: () -> [ROWUpdate (GView 'Unlocked ())] -> StateT GViewState (GView 'Unlocked) ()
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
