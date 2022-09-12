module Changes.Core.UI.View.View
    ( ViewState
    , ViewContext
    , View
    , viewOnClose
    , viewOnCloseIO
    , viewGetCloser
    , viewLiftLifecycle
    , viewLiftLifecycleWithUnlift
    , viewHoistLifecycle
    , viewSubLifecycle
    , viewGetViewState
    , viewAddViewState
    , viewWithUnliftAsync
    , viewRunResource
    , viewRunResourceContext
    , viewLocalResourceContext
    , viewGetResourceContext
    , viewWaitUpdates
    , runView
    , runNewView
    , viewBindModelUpdates
    , viewBindModel
    , viewFloatMap
    , viewFloatMapModel
    , viewBindWholeModel
    , viewBindReadOnlyWholeModel
    ) where

import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Model
import Changes.Core.Resource
import Changes.Core.Types
import Changes.Core.UI.View.Context

type ViewState = LifeState

newtype View a = MkView
    { unView :: ReaderT ViewContext Lifecycle a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadFix
               , MonadFail
               , MonadIO
               , MonadException
               , MonadThrow e
               , MonadCatch e
               , MonadHoistIO
               , MonadTunnelIO
               , MonadUnliftIO
               , RepresentationalRole
               )

viewOnCloseIO :: IO () -> View ()
viewOnCloseIO closer = viewLiftLifecycle $ lifecycleOnClose closer

viewOnClose :: View () -> View ()
viewOnClose closer = do
    vc <- MkView ask
    viewOnCloseIO $ runLifecycle $ runViewFromContext vc closer

viewGetCloser :: forall a. View a -> View (a, IO ())
viewGetCloser (MkView ma) =
    MkView $ do
        MkWUnlift unlift <- askUnlift
        lift $ lifecycleGetCloser $ unlift ma

viewGetViewState :: View a -> View (a, ViewState)
viewGetViewState (MkView ma) = MkView $ liftWithUnlift $ \unlift -> lift $ getLifeState $ unlift ma

viewAddViewState :: ViewState -> View ()
viewAddViewState vs = viewLiftLifecycle $ addLifeState vs

viewLiftLifecycle :: Lifecycle --> View
viewLiftLifecycle la = MkView $ lift la

viewLiftLifecycleWithUnlift :: ((View --> Lifecycle) -> Lifecycle a) -> View a
viewLiftLifecycleWithUnlift call = MkView $ liftWithUnlift $ \unlift -> call $ unlift . unView

viewHoistLifecycle :: (Lifecycle --> Lifecycle) -> View --> View
viewHoistLifecycle f (MkView la) = MkView $ hoist f la

viewSubLifecycle :: View --> View
viewSubLifecycle = viewHoistLifecycle $ lift . runLifecycle

viewWithUnliftAsync :: forall a. ((View --> IO) -> View a) -> View a
viewWithUnliftAsync call =
    liftIOWithUnlift $ \unlift -> unlift $ call $ unlift . viewLocalResourceContext emptyResourceContext

viewRunResource ::
       forall f r.
       Resource f
    -> (forall tt.
            (MonadTransStackUnlift tt, MonadUnliftIO (ApplyStack tt IO), MonadFail (ApplyStack tt IO)) =>
                    f tt -> ApplyStack tt IO r)
    -> View r
viewRunResource resource call = do
    rc <- viewGetResourceContext
    liftIO $ runResource rc resource $ \ftt -> call ftt

viewRunResourceContext ::
       forall f r.
       Resource f
    -> (forall tt. (MonadTransStackUnlift tt, MonadUnliftIO (ApplyStack tt View)) => StackUnlift tt -> f tt -> View r)
    -> View r
viewRunResourceContext resource call = do
    rc <- viewGetResourceContext
    runResourceContext rc resource $ \rc' unlift ftt -> viewLocalResourceContext rc' $ call unlift ftt

viewWithContext :: (ViewContext -> ViewContext) -> View --> View
viewWithContext f (MkView ma) = MkView $ withReaderT f ma

viewGetResourceContext :: View ResourceContext
viewGetResourceContext = MkView $ asks vcResourceContext

viewLocalResourceContext :: ResourceContext -> View --> View
viewLocalResourceContext rc = viewWithContext (\vc -> vc {vcResourceContext = rc})

viewWaitUpdates :: Model update -> View ()
viewWaitUpdates model = liftIO $ taskWait $ modelUpdatesTask model

runViewFromContext :: ViewContext -> View --> Lifecycle
runViewFromContext vc (MkView (ReaderT view)) = liftIOWithUnlift $ \unlift -> unlift $ view vc

runView :: View --> Lifecycle
runView = let
    vcResourceContext = emptyResourceContext
    in runViewFromContext MkViewContext {..}

runNewView :: View --> Lifecycle
runNewView = runView

viewBindModelUpdates ::
       forall update a.
       Model update
    -> (EditSource -> Bool)
    -> View a
    -> (a -> Task ())
    -> (a -> NonEmpty update -> EditContext -> View ())
    -> View a
viewBindModelUpdates model testesrc initv utask recv = do
    -- monitor makes sure updates are ignored after the view has been closed
    monitor <- viewLiftLifecycle lifecycleMonitor
    liftIOWithUnlift $ \unlift ->
        unlift $
        viewRunResourceContext model $ \stunlift amodel -> do
            a <- initv
            viewLiftLifecycle $
                stunlift $
                aModelSubscribe amodel (utask a) $ \urc updates ec@MkEditContext {..} ->
                    if testesrc editContextSource
                        then do
                            alive <- monitor
                            if alive
                                then unlift $ viewLocalResourceContext urc $ recv a updates ec
                                else return ()
                        else return ()
            return a

viewBindModel ::
       forall update a.
       Model update
    -> Maybe EditSource
    -> View a
    -> (a -> Task ())
    -> (a -> NonEmpty update -> View ())
    -> View a
viewBindModel model mesrc initv utask recv =
    viewBindModelUpdates model (\ec -> mesrc /= Just ec) initv utask $ \a updates _ec -> recv a updates

viewFloatMap ::
       forall f updateA updateB. (FloatingEditApplicative f)
    => FloatingChangeLens updateA updateB
    -> f updateA
    -> View (f updateB)
viewFloatMap flens fa = do
    rc <- viewGetResourceContext
    viewLiftLifecycle $ eaFloatMap rc flens fa

viewFloatMapModel :: forall updateA updateB. FloatingChangeLens updateA updateB -> Model updateA -> View (Model updateB)
viewFloatMapModel flens model = do
    rc <- viewGetResourceContext
    viewLiftLifecycle $ floatMapModel rc flens model

viewBindWholeModel :: forall t. Model (WholeUpdate t) -> Maybe EditSource -> (Bool -> t -> View ()) -> View ()
viewBindWholeModel model mesrc setf = let
    init :: View ()
    init =
        viewRunResourceContext model $ \unlift amodel -> do
            val <- liftIO $ unlift $ aModelRead amodel ReadWhole
            setf True val
    recv :: () -> NonEmpty (WholeUpdate t) -> View ()
    recv () updates = let
        MkWholeUpdate val = last updates
        in setf False val
    in viewBindModel model mesrc init mempty recv

viewBindReadOnlyWholeModel :: forall t. Model (ROWUpdate t) -> (Bool -> t -> View ()) -> View ()
viewBindReadOnlyWholeModel model setf = let
    init :: View ()
    init =
        viewRunResourceContext model $ \unlift amodel -> do
            val <- liftIO $ unlift $ aModelRead amodel ReadWhole
            setf True val
    recv :: () -> NonEmpty (ROWUpdate t) -> View ()
    recv () updates = let
        MkReadOnlyUpdate (MkWholeUpdate val) = last updates
        in setf False val
    in viewBindModel model Nothing init mempty recv
