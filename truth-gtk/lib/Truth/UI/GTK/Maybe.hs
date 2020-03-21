module Truth.UI.GTK.Maybe
    ( oneGetView
    ) where

import GI.Gtk hiding (get)
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

data OneWholeViews f
    = MissingOVS (Limit f)
                 ViewState
    | PresentOVS ViewState

instance DynamicViewState (OneWholeViews f) where
    dynamicViewStates (MissingOVS _ vs) = [vs]
    dynamicViewStates (PresentOVS vs) = [vs]

oneWholeView ::
       forall f update. (MonadOne f, IsUpdate update, FullEdit (UpdateEdit update))
    => Subscriber (FullResultOneUpdate f update)
    -> (f (Subscriber update) -> GCreateView)
    -> SelectNotify (f ())
    -> GCreateView
oneWholeView rmod baseView (MkSelectNotify notifyChange) = do
    let
        getWidgets :: Box -> Subscriber (FullResultOneUpdate f update) -> f () -> View (OneWholeViews f)
        getWidgets box rm fu = do
            notifyChange $ return $ Just fu
            case retrieveOne fu of
                FailureResult lfx@(MkLimit fx) -> do
                    ((), vs) <-
                        viewCreateView $ do
                            widget <- baseView fx
                            lcContainPackStart True box widget
                            widgetShow widget
                    return $ MissingOVS lfx vs
                SuccessResult () -> do
                    ((), vs) <-
                        viewCreateView $ do
                            widget <- baseView $ pure $ mapSubscriber (mustExistOneEditLens "object") rm
                            lcContainPackStart True box widget
                            widgetShow widget
                    return $ PresentOVS vs
        initVS :: Subscriber (FullResultOneUpdate f update) -> CreateView (OneWholeViews f, Box)
        initVS rm = do
            box <- new Box [#orientation := OrientationVertical]
            firstfu <- viewRunResource rm $ \am -> subRead am ReadHasOne
            vs <- cvLiftView $ getWidgets box rm firstfu
            return (vs, box)
        recvVS :: Box -> [FullResultOneUpdate f update] -> StateT (OneWholeViews f) (View) ()
        recvVS box _ = do
            olddvs <- get
            newfu <- lift $ viewRunResource rmod $ \asub -> subRead asub ReadHasOne
            case (olddvs, retrieveOne newfu) of
                (PresentOVS _, SuccessResult ()) -> return ()
                (MissingOVS _ vs, FailureResult newlf) -> put $ MissingOVS newlf vs
                _ -> replaceDynamicView $ getWidgets box rmod newfu
    box <- cvDynamic rmod initVS mempty recvVS
    toWidget box

oneGetView :: GetGView
oneGetView =
    MkGetView $ \getview uispec -> do
        OneWholeUISpec sub itemspec notifyChange <- isUISpec uispec
        return $ oneWholeView sub (\fs -> getview $ itemspec fs) notifyChange
