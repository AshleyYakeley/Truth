module Changes.Core.UI.View.View
    ( ViewState
    , ViewContext
    , View
    , viewOnClose
    , viewOnCloseIO
    , viewGetCloser
    , viewLiftLifecycle
    -- , viewLiftLifecycleWithUnlift
    , viewAskUnliftLifecycle
    , viewSubLifecycle
    , viewGetViewState
    , viewAddViewState
    , viewAskUnliftIOAsync
    , viewRunResource
    , viewRunResourceLifecycle
    , viewRunResourceContext
    , viewLocalResourceContext
    , viewGetResourceContext
    , viewWaitUpdates
    , runView
    , viewBindModelUpdates
    , viewBindModel
    , viewFloatMap
    , viewFloatMapModel
    , viewBindWholeModel
    , viewBindReadOnlyWholeModel
    )
where

import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Model
import Changes.Core.Resource
import Changes.Core.Types
import Changes.Core.UI.View.Context
import Changes.Core.UI.View.Semiview

type ViewState = LifeState Semiview

type View = LifecycleT Semiview Semiview

viewOnCloseIO :: IO () -> View ()
viewOnCloseIO closer = lifecycleOnClose $ liftIO closer

viewOnClose :: View () -> View ()
viewOnClose closer = lifecycleOnClose $ runLifecycle closer

viewGetCloser :: forall a. View a -> View (a, Semiview ())
viewGetCloser = lifecycleGetCloser

viewGetViewState :: View a -> View (a, ViewState)
viewGetViewState ma = lift $ getLifeState ma

viewAddViewState :: ViewState -> View ()
viewAddViewState = addLifeState

viewLiftLifecycle :: Lifecycle --> View
viewLiftLifecycle = hoistLifecycleBoth liftIO

viewAskUnliftLifecycle :: View (WRaised View Lifecycle)
viewAskUnliftLifecycle = do
    MkWRaised unliftIO <- lift askUnliftIO
    return $ MkWRaised $ hoistLifecycleBoth unliftIO

viewSubLifecycle :: View --> View
viewSubLifecycle ma = lift $ runLifecycle ma

viewAskUnliftIOAsync :: View (WRaised View IO)
viewAskUnliftIOAsync = do
    MkWRaised unliftIO <- askUnliftIO
    return $ MkWRaised $ unliftIO . viewLocalResourceContext emptyResourceContext

viewRunResource ::
    forall f r.
    Resource f ->
    ( forall tt.
      (MonadTransStackUnlift tt, MonadUnliftIO (ApplyStack tt IO), MonadFail (ApplyStack tt IO)) =>
      f tt -> ApplyStack tt IO r
    ) ->
    View r
viewRunResource resource call = do
    rc <- viewGetResourceContext
    liftIO $ runResource rc resource $ \ftt -> call ftt

viewRunResourceLifecycle ::
    forall f.
    MapResource f =>
    Resource f ->
    View (f '[])
viewRunResourceLifecycle resource = do
    rc <- viewGetResourceContext
    viewLiftLifecycle $ runResourceLifecycle rc resource

viewRunResourceContext ::
    forall f r.
    Resource f ->
    (forall tt. (MonadTransStackUnlift tt, MonadUnliftIO (ApplyStack tt View)) => StackUnlift tt -> f tt -> View r) ->
    View r
viewRunResourceContext resource call = do
    rc <- viewGetResourceContext
    runResourceContext rc resource $ \rc' unlift ftt -> viewLocalResourceContext rc' $ call unlift ftt

viewWithContext :: (ViewContext -> ViewContext) -> View --> View
viewWithContext f ma = hoist (semiviewWithContext f) ma

viewGetResourceContext :: View ResourceContext
viewGetResourceContext = do
    vc <- lift semiviewGetContext
    return $ vcResourceContext vc

viewLocalResourceContext :: ResourceContext -> View --> View
viewLocalResourceContext rc = viewWithContext (\vc -> vc{vcResourceContext = rc})

viewWaitUpdates :: Model update -> View ()
viewWaitUpdates model = liftIO $ taskWait $ modelUpdatesTask model

runViewFromContext :: ViewContext -> View --> Lifecycle
runViewFromContext vc ma = hoistLifecycleBoth (runSemiview vc) ma

runView :: View --> Lifecycle
runView = let
    vcResourceContext = emptyResourceContext
    in runViewFromContext MkViewContext{..}

viewBindModelUpdates ::
    forall update a.
    Model update ->
    (EditSource -> Bool) ->
    View a ->
    (a -> Task IO ()) ->
    (a -> NonEmpty update -> EditContext -> View ()) ->
    View a
viewBindModelUpdates model testesrc initv utask recv = do
    -- monitor makes sure updates are ignored after the view has been closed
    monitor <- viewLiftLifecycle lifecycleMonitor
    liftIOWithUnlift $ \unlift ->
        unlift
            $ viewRunResourceContext model
            $ \stunlift (amodel :: _ tt) -> do
                a <- initv
                Dict <- return $ transStackDict @MonadIO @tt @IO
                viewLiftLifecycle
                    $ hoist stunlift
                    $ aModelSubscribe amodel (utask a)
                    $ \urc updates ec@MkEditContext{..} ->
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
    Model update ->
    Maybe EditSource ->
    View a ->
    (a -> Task IO ()) ->
    (a -> NonEmpty update -> View ()) ->
    View a
viewBindModel model mesrc initv utask recv =
    viewBindModelUpdates model (\ec -> mesrc /= Just ec) initv utask $ \a updates _ec -> recv a updates

viewFloatMap ::
    forall f updateA updateB.
    FloatingEditApplicative f =>
    FloatingChangeLens updateA updateB ->
    f updateA ->
    View (f updateB)
viewFloatMap flens fa = do
    rc <- viewGetResourceContext
    viewLiftLifecycle $ eaFloatMap rc flens fa

viewFloatMapModel :: forall updateA updateB. FloatingChangeLens updateA updateB -> Model updateA -> View (Model updateB)
viewFloatMapModel flens model = do
    rc <- viewGetResourceContext
    viewLiftLifecycle $ floatMapModel rc flens model

viewBindWholeModel :: forall t. Model (WholeUpdate t) -> Maybe EditSource -> (Bool -> t -> View ()) -> View ()
viewBindWholeModel model mesrc setf = let
    finit :: View ()
    finit =
        viewRunResourceContext model $ \unlift amodel -> do
            val <- liftIO $ unlift $ aModelRead amodel ReadWhole
            setf True val
    recv :: () -> NonEmpty (WholeUpdate t) -> View ()
    recv () updates = let
        MkWholeUpdate val = last updates
        in setf False val
    in viewBindModel model mesrc finit mempty recv

viewBindReadOnlyWholeModel :: forall t. Model (ROWUpdate t) -> (Bool -> t -> View ()) -> View ()
viewBindReadOnlyWholeModel model setf = let
    finit :: View ()
    finit =
        viewRunResourceContext model $ \unlift amodel -> do
            val <- liftIO $ unlift $ aModelRead amodel ReadWhole
            setf True val
    recv :: () -> NonEmpty (ROWUpdate t) -> View ()
    recv () updates = let
        MkReadOnlyUpdate (MkWholeUpdate val) = last updates
        in setf False val
    in viewBindModel model Nothing finit mempty recv
