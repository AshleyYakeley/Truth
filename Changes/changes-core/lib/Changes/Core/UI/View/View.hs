module Changes.Core.UI.View.View
    ( ViewState
    , ViewContext
    , View
    , viewOnClose
    , viewOnCloseIO
    , viewGetCloser
    , viewLiftLifeCycle
    , viewLiftLifeCycleWithUnlift
    , viewHoistLifeCycle
    , viewSubLifeCycle
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
    { unView :: ReaderT ViewContext LifeCycle a
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
viewOnCloseIO closer = viewLiftLifeCycle $ lifeCycleOnClose closer

viewOnClose :: View () -> View ()
viewOnClose closer = do
    vc <- MkView ask
    viewOnCloseIO $ runLifeCycleT $ runViewFromContext vc closer

viewGetCloser :: forall a. View a -> View (a, IO ())
viewGetCloser (MkView ma) =
    MkView $ do
        MkWUnlift unlift <- askUnlift
        lift $ lifeCycleGetCloser $ unlift ma

viewGetViewState :: View a -> View (a, ViewState)
viewGetViewState (MkView ma) = MkView $ liftWithUnlift $ \unlift -> lift $ getLifeState $ unlift ma

viewAddViewState :: ViewState -> View ()
viewAddViewState vs = viewLiftLifeCycle $ addLifeState vs

viewLiftLifeCycle :: LifeCycle --> View
viewLiftLifeCycle la = MkView $ lift la

viewLiftLifeCycleWithUnlift :: ((View --> LifeCycle) -> LifeCycle a) -> View a
viewLiftLifeCycleWithUnlift call = MkView $ liftWithUnlift $ \unlift -> call $ unlift . unView

viewHoistLifeCycle :: (LifeCycle --> LifeCycle) -> View --> View
viewHoistLifeCycle f (MkView la) = MkView $ hoist f la

viewSubLifeCycle :: View --> View
viewSubLifeCycle = viewHoistLifeCycle $ lift . runLifeCycleT

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
    -> (forall tt. (MonadTransStackUnlift tt, MonadUnliftIO (ApplyStack tt View)) => StackUnliftAll tt -> f tt -> View r)
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

runViewFromContext :: ViewContext -> View --> LifeCycle
runViewFromContext vc (MkView (ReaderT view)) = liftIOWithUnlift $ \unlift -> unlift $ view vc

runView :: View --> LifeCycle
runView = let
    vcResourceContext = emptyResourceContext
    in runViewFromContext MkViewContext {..}

runNewView :: View --> LifeCycle
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
    monitor <- viewLiftLifeCycle lifeCycleMonitor
    liftIOWithUnlift $ \unlift ->
        unlift $
        viewRunResourceContext model $ \stunlift amodel -> do
            a <- initv
            viewLiftLifeCycle $
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
    viewLiftLifeCycle $ eaFloatMap rc flens fa

viewFloatMapModel :: forall updateA updateB. FloatingChangeLens updateA updateB -> Model updateA -> View (Model updateB)
viewFloatMapModel flens model = do
    rc <- viewGetResourceContext
    viewLiftLifeCycle $ floatMapModel rc flens model

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
