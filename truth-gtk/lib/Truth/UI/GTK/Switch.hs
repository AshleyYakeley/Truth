module Truth.UI.GTK.Switch
    ( switchView
    , switchGetView
    ) where

import GI.Gtk hiding (get)
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

switchView :: forall sel edit. UpdateFunction edit (WholeUpdate (GCreateView sel edit)) -> GCreateView sel edit
switchView specfunc = do
    box <- liftIO $ boxNew OrientationVertical 0
    let
        getViewState :: GCreateView sel edit -> View sel edit (ViewState sel)
        getViewState gview =
            viewCreateView $ do
                widget <- gview
                lcContainPackStart True box widget
                #show widget
    firstvs <- do
        firstspec <-
            cvMapEdit (return $ readOnlyEditLens specfunc) $ cvLiftView $ viewObjectRead $ \_ mr -> mr ReadWhole
        cvLiftView $ getViewState firstspec
    unliftView <- cvLiftView askUnliftIO
    cvDynamic @(ViewState sel) firstvs $ \object updates -> do
        whupdates <- liftIO $ objectMapUpdates specfunc object updates
        for_ (lastWholeUpdate whupdates) $ \spec -> do
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
                MkSwitchUISpec specfunc -> switchView $ funcUpdateFunction getview . specfunc
