module Changes.Core.UI.View.CreateView
    ( CreateView
    , ViewState
    , cvLiftViewWithUnlift
    , cvBindModelUpdates
    , cvBindModel
    , cvFloatMapModel
    , cvBindWholeModel
    , cvBindReadOnlyWholeModel
    ) where

import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Model
import Changes.Core.Types
import Changes.Core.UI.View.Context
import Changes.Core.UI.View.View

type CreateView = ViewT LifeCycle

type ViewState = LifeState

cvLiftViewWithUnlift :: View -/-> CreateView
cvLiftViewWithUnlift = backHoist liftIOWithUnlift

cvBindModelUpdates ::
       forall update a.
       Model update
    -> (EditSource -> Bool)
    -> CreateView a
    -> (a -> Task ())
    -> (a -> NonEmpty update -> EditContext -> View ())
    -> CreateView a
cvBindModelUpdates model testesrc initv utask recv = do
    -- monitor makes sure updates are ignored after the view has been closed
    monitor <- liftLifeCycle lifeCycleMonitor
    withUILock <- asks $ \vc -> vcWithUILock vc
    unliftView <- liftToLifeCycle askUnliftIO
    viewRunResourceContext model $ \unlift amodel -> do
        a <- initv
        liftLifeCycle $
            unlift $
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

cvBindModel ::
       forall update a.
       Model update
    -> Maybe EditSource
    -> CreateView a
    -> (a -> Task ())
    -> (a -> NonEmpty update -> View ())
    -> CreateView a
cvBindModel model mesrc initv utask recv =
    cvBindModelUpdates model (\ec -> mesrc /= Just ec) initv utask $ \a updates _ec -> recv a updates

cvFloatMapModel ::
       forall updateA updateB. FloatingChangeLens updateA updateB -> Model updateA -> CreateView (Model updateB)
cvFloatMapModel flens model = do
    rc <- viewGetResourceContext
    liftLifeCycle $ floatMapModel rc flens model

cvBindWholeModel :: forall t. Model (WholeUpdate t) -> Maybe EditSource -> (t -> View ()) -> CreateView ()
cvBindWholeModel model mesrc setf = let
    init :: CreateView ()
    init =
        viewRunResourceContext model $ \unlift amodel -> do
            val <- liftIO $ unlift $ aModelRead amodel ReadWhole
            liftToLifeCycle $ setf val
    recv :: () -> NonEmpty (WholeUpdate t) -> View ()
    recv () updates = let
        MkWholeUpdate val = last updates
        in setf val
    in cvBindModel model mesrc init mempty recv

cvBindReadOnlyWholeModel :: forall t. Model (ROWUpdate t) -> (t -> View ()) -> CreateView ()
cvBindReadOnlyWholeModel model setf = let
    init :: CreateView ()
    init =
        viewRunResourceContext model $ \unlift amodel -> do
            val <- liftIO $ unlift $ aModelRead amodel ReadWhole
            liftToLifeCycle $ setf val
    recv :: () -> NonEmpty (ROWUpdate t) -> View ()
    recv () updates = let
        MkReadOnlyUpdate (MkWholeUpdate val) = last updates
        in setf val
    in cvBindModel model Nothing init mempty recv
