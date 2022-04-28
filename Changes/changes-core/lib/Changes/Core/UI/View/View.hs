module Changes.Core.UI.View.View
    ( ViewState
    , ViewContext
    , View
    , viewOnClose
    , viewOnCloseIO
    , viewGetCloser
    , viewLiftLifeCycle
    , viewSubLifeCycle
    , viewRunInMain
    , viewGetState
    , liftIOViewAsync
    , viewRunResource
    , viewRunResourceContext
    , viewLocalResourceContext
    , viewGetResourceContext
    , viewWithoutLock
    , viewWaitUpdates
    , runView
    , viewExitUI
    , viewBindModelUpdates
    , viewBindModel
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
import Changes.Debug.Reference

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

viewOnClose :: View () -> View ()
viewOnClose (MkView closer) = MkView $ liftWithUnlift $ \unlift -> lifeCycleOnClose $ runLifeCycleT $ unlift closer

viewOnCloseIO :: IO () -> View ()
viewOnCloseIO closer = viewLiftLifeCycle $ lifeCycleOnClose closer

viewGetCloser :: forall a. View a -> View (a, IO ())
viewGetCloser (MkView ma) =
    MkView $ do
        MkWUnlift unlift <- askUnlift
        lift $ lifeCycleGetCloser $ unlift ma

viewGetState :: View a -> View (a, ViewState)
viewGetState (MkView ma) = MkView $ liftWithUnlift $ \unlift -> liftIO $ getLifeState $ unlift ma

viewLiftLifeCycle :: LifeCycle --> View
viewLiftLifeCycle la = MkView $ lift la

viewSubLifeCycle :: View --> View
viewSubLifeCycle (MkView la) = MkView $ hoist (lift . runLifeCycleT) la

viewRunInMain :: View (WMFunction View IO)
viewRunInMain =
    MkView $ do
        vc <- ask
        MkWUnlift unlift <- askUnlift
        return $ MkWMFunction $ vcRunInMain vc . unlift . unView

liftIOViewAsync :: forall a. ((forall r. View r -> IO r) -> IO a) -> View a
liftIOViewAsync call = do
    MkWMFunction run <- viewRunInMain
    liftIO $ call $ run . viewLocalResourceContext emptyResourceContext

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

viewWithoutLock :: IO --> View
viewWithoutLock ioa = do
    withoutLock <- MkView $ asks $ \vc -> vcWithoutUILock vc
    liftIO $ withoutLock ioa

viewWaitUpdates :: Model update -> View ()
viewWaitUpdates model = viewWithoutLock $ taskWait $ modelUpdatesTask model

runView :: ViewContext -> View --> LifeCycle
runView vc (MkView (ReaderT view)) = liftIOWithUnlift $ \unlift -> vcWithUILock vc $ unlift $ view vc

-- | Stop the UI loop. This does not throw any kind of exception.
viewExitUI :: View ()
viewExitUI = traceBracket "viewExit" $ do
    exit <- MkView $ asks vcExit
    liftIO exit

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
    withUILock <- MkView $ asks $ \vc -> vcWithUILock vc
    unliftView <- viewRunInMain
    viewRunResourceContext model $ \stunlift amodel -> do
        a <- initv
        viewLiftLifeCycle $
            stunlift $
            aModelSubscribe amodel (utask a) $ \urc updates ec@MkEditContext {..} ->
                if testesrc editContextSource
                    then withUILock $ do
                             alive <- monitor
                             if alive
                                 then do
                                     runWMFunction unliftView $ viewLocalResourceContext urc $ recv a updates ec
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

viewFloatMapModel :: forall updateA updateB. FloatingChangeLens updateA updateB -> Model updateA -> View (Model updateB)
viewFloatMapModel flens model = do
    rc <- viewGetResourceContext
    viewLiftLifeCycle $ floatMapModel rc flens model

viewBindWholeModel :: forall t. Model (WholeUpdate t) -> Maybe EditSource -> (t -> View ()) -> View ()
viewBindWholeModel model mesrc setf = let
    init :: View ()
    init =
        viewRunResourceContext model $ \unlift amodel -> do
            val <- liftIO $ unlift $ aModelRead amodel ReadWhole
            setf val
    recv :: () -> NonEmpty (WholeUpdate t) -> View ()
    recv () updates = let
        MkWholeUpdate val = last updates
        in setf val
    in viewBindModel model mesrc init mempty recv

viewBindReadOnlyWholeModel :: forall t. Model (ROWUpdate t) -> (t -> View ()) -> View ()
viewBindReadOnlyWholeModel model setf = let
    init :: View ()
    init =
        viewRunResourceContext model $ \unlift amodel -> do
            val <- liftIO $ unlift $ aModelRead amodel ReadWhole
            setf val
    recv :: () -> NonEmpty (ROWUpdate t) -> View ()
    recv () updates = let
        MkReadOnlyUpdate (MkWholeUpdate val) = last updates
        in setf val
    in viewBindModel model Nothing init mempty recv
