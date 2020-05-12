module Truth.UI.GTK.Switch
    ( switchView
    , switchGetView
    ) where

import GI.Gtk hiding (get)
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful
import Truth.Debug.Object

switchView :: Model (ROWUpdate GCreateView) -> GCreateView
switchView sub = do
    box <- liftIO $ boxNew OrientationVertical 0
    let
        getViewState :: GCreateView -> View ViewState
        getViewState gview = do
            ((), vs) <-
                viewCreateView $ do
                    widget <- traceBracket "GTK.Switch:getViewState.gview" gview
                    lcContainPackStart True box widget
                    #show widget
            return vs
        initVS :: Model (ROWUpdate GCreateView) -> CreateView (ViewState, ())
        initVS rm = do
            firstspec <- viewRunResource rm $ \am -> aModelRead am ReadWhole
            vs <- cvLiftView $ getViewState firstspec
            return (vs, ())
        recvVS :: () -> [ROWUpdate GCreateView] -> StateT ViewState View ()
        recvVS () updates = traceBracket "GTK.Switch:update" $ for_ (lastReadOnlyWholeUpdate updates) $ \spec -> replaceDynamicView $ getViewState spec
    cvDynamic @(ViewState) sub initVS mempty recvVS
    toWidget box

switchGetView :: GetGView
switchGetView =
    MkGetView $ \getview uispec -> do
        spec <- isUISpec uispec
        return $
            case spec of
                MkSwitchUISpec sub -> switchView $ mapModel (liftReadOnlyChangeLens $ funcChangeLens getview) sub
