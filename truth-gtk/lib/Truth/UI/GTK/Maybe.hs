module Truth.UI.GTK.Maybe
    ( createOneWhole
    , createOneWholSel
    ) where

import GI.Gtk hiding (get)
import Shapes
import Truth.Core
import Truth.UI.GTK.Useful

data OneWholeViews f
    = MissingOVS (f None)
                 ViewState
    | PresentOVS ViewState

instance DynamicViewState (OneWholeViews f) where
    dynamicViewStates (MissingOVS _ vs) = [vs]
    dynamicViewStates (PresentOVS vs) = [vs]

oneWholeView ::
       forall f update. (MonadOne f, IsUpdate update, FullEdit (UpdateEdit update))
    => Model (FullResultOneUpdate f update)
    -> (f (Model update) -> CreateView Widget)
    -> SelectNotify (f ())
    -> CreateView Widget
oneWholeView rmod baseView (MkSelectNotify notifyChange) = do
    let
        getWidgets :: Box -> Model (FullResultOneUpdate f update) -> f () -> View (OneWholeViews f)
        getWidgets box rm fu = do
            notifyChange $ return $ Just fu
            case retrieveOne fu of
                FailureResult fn -> do
                    ((), vs) <-
                        viewCreateView $ do
                            widget <- baseView $ fmap never fn
                            cvPackStart True box widget
                            widgetShow widget
                    return $ MissingOVS fn vs
                SuccessResult () -> do
                    ((), vs) <-
                        viewCreateView $ do
                            widget <- baseView $ pure $ mapModel (mustExistOneChangeLens "reference") rm
                            cvPackStart True box widget
                            widgetShow widget
                    return $ PresentOVS vs
        initVS :: Model (FullResultOneUpdate f update) -> CreateView (OneWholeViews f, Box)
        initVS rm = do
            box <- cvNew Box [#orientation := OrientationVertical]
            firstfu <- viewRunResource rm $ \am -> aModelRead am ReadHasOne
            vs <- cvLiftView $ getWidgets box rm firstfu
            return (vs, box)
        recvVS :: Box -> [FullResultOneUpdate f update] -> StateT (OneWholeViews f) (View) ()
        recvVS box _ = do
            olddvs <- get
            newfu <- lift $ viewRunResource rmod $ \asub -> aModelRead asub ReadHasOne
            case (olddvs, retrieveOne newfu) of
                (PresentOVS _, SuccessResult ()) -> return ()
                (MissingOVS _ vs, FailureResult newlf) -> put $ MissingOVS newlf vs
                _ -> replaceDynamicView $ getWidgets box rmod newfu
    box <- cvDynamic rmod initVS mempty recvVS
    toWidget box

createOneWhole ::
       forall f update. (IsUpdate update, MonadOne f, FullEdit (UpdateEdit update))
    => Model (FullResultOneUpdate f update)
    -> (f (Model update) -> CreateView Widget)
    -> CreateView Widget
createOneWhole sub itemspec = oneWholeView sub itemspec mempty

createOneWholSel ::
       forall sel f update. (IsUpdate update, MonadOne f, FullEdit (UpdateEdit update))
    => Model (FullResultOneUpdate f update)
    -> (f (Model update, SelectNotify sel) -> CreateView Widget)
    -> SelectNotify (f sel)
    -> CreateView Widget
createOneWholSel subf specsel snfsel = let
    spec :: f (Model update) -> CreateView Widget
    spec fsub = specsel $ fmap (\sub -> (sub, contramap pure snfsel)) fsub
    getf :: f () -> Maybe (f sel)
    getf fu =
        case retrieveOne fu of
            SuccessResult _ -> Nothing
            FailureResult fn -> Just $ fmap never fn
    snfu :: SelectNotify (f ())
    snfu = mapMaybeSelectNotify getf snfsel
    in oneWholeView subf spec snfu
