module Truth.Core.UI.View.CreateView
    ( CreateView
    , ViewState
    , viewCreateView
    , cvEarlyCloser
    , cvLiftView
    , cvBindSubscriber
    , cvFloatMapSubscriber
    , cvBindWholeSubscriber
    , cvBindReadOnlyWholeSubscriber
    ) where

import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Object
import Truth.Core.Types
import Truth.Core.UI.View.Context
import Truth.Core.UI.View.View

type CreateView = ViewT LifeCycleIO

type ViewState = LifeState IO

viewCreateView :: CreateView t -> View (t, ViewState)
viewCreateView (ReaderT wff) = ReaderT $ \vc -> getLifeState $ wff vc

cvEarlyCloser :: CreateView a -> CreateView (a, IO ())
cvEarlyCloser ca = liftWithUnlift $ \unlift -> lifeCycleEarlyCloser $ unlift ca

cvLiftView :: View a -> CreateView a
cvLiftView = remonad liftIO

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
    withUILock <- asks vcWithUILock
    unliftView <- cvLiftView askUnliftIO
    viewRunResourceContext model $ \unlift (amodel :: _ tt) -> do
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
                                     runWMFunction unliftView $ viewLocalResourceContext urc $ recv a updates
                                 else return ()
        return a

cvFloatMapSubscriber ::
       forall updateA updateB. FloatingEditLens updateA updateB -> Subscriber updateA -> CreateView (Subscriber updateB)
cvFloatMapSubscriber flens model = do
    rc <- viewGetResourceContext
    liftLifeCycleIO $ floatMapSubscriber rc flens model

cvBindWholeSubscriber :: forall t. Subscriber (WholeUpdate t) -> Maybe EditSource -> (t -> View ()) -> CreateView ()
cvBindWholeSubscriber sub mesrc setf = let
    init :: Subscriber (WholeUpdate t) -> CreateView ()
    init rmod =
        viewRunResourceContext rmod $ \unlift (amod :: _ tt) -> do
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
        viewRunResourceContext rmod $ \unlift (amod :: _ tt) -> do
            val <- liftIO $ unlift $ subRead amod ReadWhole
            cvLiftView $ setf val
    recv :: () -> NonEmpty (ROWUpdate t) -> View ()
    recv () updates = let
        MkReadOnlyUpdate (MkWholeUpdate val) = last updates
        in setf val
    in cvBindSubscriber sub Nothing init mempty recv
