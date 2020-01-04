module Truth.UI.GTK.Switch
    ( switchView
    , switchGetView
    ) where

import GI.Gtk hiding (get)
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

switchView :: forall sel. ReadOnlyOpenSubscriber (WholeUpdate (GCreateView sel)) -> GCreateView sel
switchView sub = do
    box <- liftIO $ boxNew OrientationVertical 0
    unliftView <- cvLiftView askUnliftIO
    let
        getViewState :: GCreateView sel -> View sel (ViewState sel)
        getViewState gview =
            viewCreateView $ do
                widget <- gview
                lcContainPackStart True box widget
                #show widget
        initVS :: ReadOnlyOpenSubscriber (WholeUpdate (GCreateView sel)) -> CreateView sel (ViewState sel)
        initVS rm = do
            firstspec <- liftIO $ withOpenResource rm $ \am -> subRead am ReadWhole
            cvLiftView $ getViewState firstspec
        recvVS :: [ReadOnlyUpdate (WholeUpdate (GCreateView sel))] -> StateT (ViewState sel) IO ()
        recvVS updates =
            for_ (lastWholeUpdate $ fmap unReadOnlyUpdate updates) $ \spec ->
                replaceDynamicView $ runWMFunction unliftView $ getViewState spec
    cvDynamic @(ViewState sel) sub initVS recvVS
    toWidget box

switchGetView :: GetGView
switchGetView =
    MkGetView $ \getview uispec -> do
        spec <- isUISpec uispec
        return $
            case spec of
                MkSwitchUISpec sub -> switchView $ mapReadOnlyWholeOpenSubscriber getview sub
