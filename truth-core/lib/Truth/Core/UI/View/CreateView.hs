module Truth.Core.UI.View.CreateView
    ( CreateView
    , ViewState
    , viewCreateView
    , cvEarlyCloser
    , cvLiftView
    , cvUnliftView
    , cvGetResourceContext
    , cvRunResource
    , cvRunResourceContext
    , cvBindSubscriber
    , cvFloatMapSubscriber
    , cvBindWholeSubscriber
    , cvBindReadOnlyWholeSubscriber
    , runCreateView
    ) where

import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Object
import Truth.Core.Resource
import Truth.Core.Types
import Truth.Core.UI.View.Context
import Truth.Core.UI.View.View

newtype CreateView a = MkCreateView
    { unCreateView :: ReaderT ViewContext LifeCycleIO a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadFix, MonadUnliftIO)

type ViewState = LifeState IO ()

viewCreateView :: CreateView () -> View ViewState
viewCreateView (MkCreateView (ReaderT wff)) = MkView $ ReaderT $ \vc -> getLifeState $ wff vc

cvEarlyCloser :: CreateView a -> CreateView (a, IO ())
cvEarlyCloser (MkCreateView ca) = MkCreateView $ liftWithUnlift $ \unlift -> lifeCycleEarlyCloser $ unlift ca

cvLiftView :: View a -> CreateView a
cvLiftView (MkView (ReaderT va)) = MkCreateView $ ReaderT $ \vc -> liftIO $ va vc

cvUnliftView :: MFunction LifeCycleIO IO -> CreateView a -> View a
cvUnliftView mf (MkCreateView rt) = MkView $ remonad mf rt

instance MonadLifeCycleIO CreateView where
    liftLifeCycleIO lc = MkCreateView $ lift lc

instance MonadUnliftLifeCycleIO CreateView where
    liftLifeCycleIOWithUnlift call = MkCreateView $ liftWithUnlift $ \unliftR -> call $ unliftR . unCreateView

cvWithContext :: (ViewContext -> ViewContext) -> CreateView a -> CreateView a
cvWithContext f (MkCreateView ma) = MkCreateView $ withReaderT f ma

cvGetResourceContext :: CreateView ResourceContext
cvGetResourceContext = MkCreateView $ asks vcResourceContext

cvRunResource ::
       forall f r.
       Resource f
    -> (forall tt. (MonadTransStackUnliftAll tt, MonadUnliftIO (ApplyStack tt IO)) => f tt -> ApplyStack tt IO r)
    -> CreateView r
cvRunResource resource call = do
    rc <- cvGetResourceContext
    liftIO $ runResource rc resource $ \ftt -> call ftt

cvRunResourceContext ::
       forall f r.
       Resource f
    -> (forall tt.
            (MonadTransStackUnliftAll tt, MonadUnliftIO (ApplyStack tt CreateView)) =>
                    StackUnliftAll tt -> f tt -> CreateView r)
    -> CreateView r
cvRunResourceContext resource call = do
    rc <- cvGetResourceContext
    runResourceContext rc resource $ \rc' unlift ftt ->
        cvWithContext (\vc -> vc {vcResourceContext = rc'}) $ call unlift ftt

cvBindSubscriber ::
       forall update a.
       Subscriber update
    -> Maybe EditSource
    -> (Subscriber update -> CreateView a)
    -> Task ()
    -> (a -> NonEmpty update -> View ())
    -> CreateView a
cvBindSubscriber model mesrc initv utask recv = do
    -- monitor makes sure updates are ignored after the view has been closed
    monitor <- liftLifeCycleIO lifeCycleMonitor
    withUILock <- MkCreateView $ asks vcWithUILock
    unliftView <- cvLiftView askUnliftIO
    cvRunResourceContext model $ \unlift (amodel :: _ tt) -> do
        a <- initv model
        liftLifeCycleIO $
            unlift $
            subscribe amodel utask $ \urc updates MkEditContext {..} ->
                if mesrc == Just editContextSource
                    then return ()
                    else withUILock $ do
                             alive <- monitor
                             if alive
                                 then do
                                     runWMFunction unliftView $
                                         viewWithContext (\vc -> vc {vcResourceContext = urc}) $ recv a updates
                                 else return ()
        return a

cvFloatMapSubscriber ::
       forall updateA updateB. FloatingEditLens updateA updateB -> Subscriber updateA -> CreateView (Subscriber updateB)
cvFloatMapSubscriber flens model = do
    rc <- cvGetResourceContext
    liftLifeCycleIO $ floatMapSubscriber rc flens model

cvBindWholeSubscriber :: forall t. Subscriber (WholeUpdate t) -> Maybe EditSource -> (t -> View ()) -> CreateView ()
cvBindWholeSubscriber sub mesrc setf = let
    init :: Subscriber (WholeUpdate t) -> CreateView ()
    init rmod =
        cvRunResourceContext rmod $ \unlift (amod :: _ tt) -> do
            val <- liftIO $ unlift $ subRead amod ReadWhole
            cvLiftView $ setf val
    recv :: () -> NonEmpty (WholeUpdate t) -> View ()
    recv () updates = let
        MkWholeUpdate val = last updates
        in setf val
    in cvBindSubscriber sub mesrc init mempty recv

cvBindReadOnlyWholeSubscriber :: forall t. Subscriber (ROWUpdate t) -> (t -> View ()) -> CreateView ()
cvBindReadOnlyWholeSubscriber sub setf = let
    init :: Subscriber (ROWUpdate t) -> CreateView ()
    init rmod =
        cvRunResourceContext rmod $ \unlift (amod :: _ tt) -> do
            val <- liftIO $ unlift $ subRead amod ReadWhole
            cvLiftView $ setf val
    recv :: () -> NonEmpty (ROWUpdate t) -> View ()
    recv () updates = let
        MkReadOnlyUpdate (MkWholeUpdate val) = last updates
        in setf val
    in cvBindSubscriber sub Nothing init mempty recv

runCreateView ::
       forall w.
       ResourceContext
    -> (IO () -> IO ())
    -> CreateView w
    -> (forall t. IOWitness t -> Maybe t)
    -> LifeCycleIO w
runCreateView vcResourceContext vcWithUILock (MkCreateView (ReaderT view)) vcRequest = view MkViewContext {..}
