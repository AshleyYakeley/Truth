module Truth.Core.UI.View.CreateView
    ( CreateView
    , ViewState
    , viewCreateView
    , cvEarlyCloser
    , cvLiftView
    , cvBindModel
    , cvFloatMapModel
    , cvBindWholeModel
    , cvBindReadOnlyWholeModel
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

cvBindModel ::
       forall update a.
       Model update
    -> Maybe EditSource
    -> (Model update -> CreateView a)
    -> Task ()
    -> (a -> NonEmpty update -> View ())
    -> CreateView a
cvBindModel model mesrc initv utask recv = do
    -- monitor makes sure updates are ignored after the view has been closed
    monitor <- liftLifeCycleIO lifeCycleMonitor
    withUILock <- asks vcWithUILock
    unliftView <- cvLiftView askUnliftIO
    viewRunResourceContext model $ \unlift (amodel :: _ tt) -> do
        a <- initv model
        liftLifeCycleIO $
            unlift $
            aModelSubscribe amodel utask $ \urc updates MkEditContext {..} ->
                if mesrc == Just editContextSource
                    then return ()
                    else withUILock $ do
                             alive <- monitor
                             if alive
                                 then do
                                     runWMFunction unliftView $ viewLocalResourceContext urc $ recv a updates
                                 else return ()
        return a

cvFloatMapModel ::
       forall updateA updateB. FloatingEditLens updateA updateB -> Model updateA -> CreateView (Model updateB)
cvFloatMapModel flens model = do
    rc <- viewGetResourceContext
    liftLifeCycleIO $ floatMapModel rc flens model

cvBindWholeModel :: forall t. Model (WholeUpdate t) -> Maybe EditSource -> (t -> View ()) -> CreateView ()
cvBindWholeModel sub mesrc setf = let
    init :: Model (WholeUpdate t) -> CreateView ()
    init rmod =
        viewRunResourceContext rmod $ \unlift (amod :: _ tt) -> do
            val <- liftIO $ unlift $ aModelRead amod ReadWhole
            cvLiftView $ setf val
    recv :: () -> NonEmpty (WholeUpdate t) -> View ()
    recv () updates = let
        MkWholeUpdate val = last updates
        in setf val
    in cvBindModel sub mesrc init mempty recv

cvBindReadOnlyWholeModel :: forall t. Model (ROWUpdate t) -> (t -> View ()) -> CreateView ()
cvBindReadOnlyWholeModel sub setf = let
    init :: Model (ROWUpdate t) -> CreateView ()
    init rmod =
        viewRunResourceContext rmod $ \unlift (amod :: _ tt) -> do
            val <- liftIO $ unlift $ aModelRead amod ReadWhole
            cvLiftView $ setf val
    recv :: () -> NonEmpty (ROWUpdate t) -> View ()
    recv () updates = let
        MkReadOnlyUpdate (MkWholeUpdate val) = last updates
        in setf val
    in cvBindModel sub Nothing init mempty recv
