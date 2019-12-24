module Truth.UI.GTK.Switch
    ( switchView
    , switchGetView
    ) where

import GI.Gtk hiding (get)
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

switchView :: forall sel. ReadOnlySubscriber (WholeUpdate (GCreateView sel)) -> GCreateView sel
switchView sub =
    runResource sub $ \run asub -> do
        box <- liftIO $ boxNew OrientationVertical 0
        let
            getViewState :: GCreateView sel -> View sel (ViewState sel)
            getViewState gview =
                viewCreateView $ do
                    widget <- gview
                    lcContainPackStart True box widget
                    #show widget
        firstvs <- do
            firstspec <- liftIO $ run $ subRead asub ReadWhole
            cvLiftView $ getViewState firstspec
        unliftView <- cvLiftView askUnliftIO
        cvDynamic @(ViewState sel) sub firstvs $ \updates ->
            for_ (lastWholeUpdate $ fmap unReadOnlyUpdate updates) $ \spec -> do
                oldvs <- get
                liftIO $ closeDynamicView oldvs
                newvs <- liftIO $ runWMFunction unliftView $ getViewState spec
                put newvs
        toWidget box

switchGetView :: GetGView
switchGetView =
    MkGetView $ \getview uispec -> do
        spec <- isUISpec uispec
        return $
            case spec of
                MkSwitchUISpec sub -> switchView $ mapReadOnlySubscriber (funcUpdateFunction getview) sub
