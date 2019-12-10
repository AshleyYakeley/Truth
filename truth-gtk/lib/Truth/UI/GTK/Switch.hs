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

switchView :: forall sel edit. UpdateFunction edit (WholeUpdate (GCreateView sel edit)) -> GCreateView sel edit
switchView specfunc = do
    box <- liftIO $ boxNew OrientationVertical 0
    let
        getViewState :: GCreateView sel edit -> View sel edit (ViewState sel)
        getViewState gview =
            viewCreateView $ do
                widget <- traceBracket "GTK.Switch:getViewState.gview" $ gview
                lcContainPackStart True box widget
                #show widget
    firstvs <- do
        firstspec <-
            cvMapEdit (return $ readOnlyEditLens specfunc) $ cvLiftView $ viewObjectRead $ \_ mr -> mr ReadWhole
        cvLiftView $ getViewState firstspec
    unliftView <- cvLiftView askUnliftIO
    cvDynamic @(ViewState sel) firstvs $ \object updates -> traceBracket "GTK.Switch:update" $
        case nonEmpty updates of
            Nothing -> return ()
            Just updates' -> do
                whupdates <- liftIO $ objectMapUpdates specfunc object updates'
                for_ (lastWholeUpdate whupdates) $ \spec -> do
                    oldvs <- get
                    liftIO $ closeDynamicView oldvs
                    newvs <- liftIO $ runWMFunction (traceThing "GTK.Switch:update.getViewState" unliftView) $ getViewState spec
                    put newvs
    toWidget box

switchGetView :: GetGView
switchGetView =
    MkGetView $ \getview uispec -> do
        spec <- isUISpec uispec
        return $
            case spec of
                MkSwitchUISpec specfunc -> switchView $ funcUpdateFunction getview . specfunc
