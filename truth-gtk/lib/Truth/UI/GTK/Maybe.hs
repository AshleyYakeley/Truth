module Truth.UI.GTK.Maybe
    ( oneGetView
    ) where

import GI.Gtk hiding (get)
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

data OneWholeViews sel f
    = MissingOVS (Limit f)
                 (ViewState sel)
    | PresentOVS (ViewState sel)

instance DynamicViewState (OneWholeViews sel f) where
    type DynamicViewSelEdit (OneWholeViews sel f) = sel
    dynamicViewStates (MissingOVS _ vs) = [vs]
    dynamicViewStates (PresentOVS vs) = [vs]
    dynamicViewFocus (MissingOVS _ vs) = Just vs
    dynamicViewFocus (PresentOVS vs) = Just vs

oneWholeView ::
       forall sel f update. (MonadOne f, IsUpdate update, FullEdit (UpdateEdit update))
    => OpenSubscriber (FullResultOneUpdate f update)
    -> (f (OpenSubscriber update) -> GCreateView sel)
    -> GCreateView sel
oneWholeView rmod baseView = do
    unliftView <- cvLiftView askUnliftIO
    let
        getWidgets :: Box -> OpenSubscriber (FullResultOneUpdate f update) -> f () -> View sel (OneWholeViews sel f)
        getWidgets box rm fu =
            case retrieveOne fu of
                FailureResult lfx@(MkLimit fx) -> do
                    vs <-
                        viewCreateView $ do
                            widget <- baseView fx
                            lcContainPackStart True box widget
                            widgetShow widget
                    return $ MissingOVS lfx vs
                SuccessResult () -> do
                    vs <-
                        viewCreateView $ do
                            widget <- baseView $ pure $ mapOpenSubscriber (mustExistOneEditLens "object") rm
                            lcContainPackStart True box widget
                            widgetShow widget
                    return $ PresentOVS vs
        initVS :: OpenSubscriber (FullResultOneUpdate f update) -> CreateView sel (OneWholeViews sel f, Box)
        initVS rm = do
            box <- new Box [#orientation := OrientationVertical]
            firstfu <- liftIO $ withOpenResource rm $ \am -> subRead am ReadHasOne
            vs <- cvLiftView $ getWidgets box rm firstfu
            return (vs, box)
        recvVS :: Box -> [FullResultOneUpdate f update] -> StateT (OneWholeViews sel f) IO ()
        recvVS box _ = do
            olddvs <- get
            newfu <- liftIO $ withOpenResource rmod $ \asub -> subRead asub ReadHasOne
            case (olddvs, retrieveOne newfu) of
                (PresentOVS _, SuccessResult ()) -> return ()
                (MissingOVS _ vs, FailureResult newlf) -> put $ MissingOVS newlf vs
                _ -> replaceDynamicView $ runWMFunction unliftView $ getWidgets box rmod newfu
    box <- cvDynamic rmod initVS mempty recvVS
    toWidget box

oneGetView :: GetGView
oneGetView =
    MkGetView $ \getview uispec -> do
        OneWholeUISpec sub itemspec <- isUISpec uispec
        return $ oneWholeView sub $ \fs -> getview $ itemspec fs
