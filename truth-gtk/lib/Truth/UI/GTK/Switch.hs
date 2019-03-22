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

switchView :: forall sel edit. EditFunction edit (WholeEdit (GCreateView sel edit)) -> GCreateView sel edit
switchView specfunc = do
    box <- liftIO $ boxNew OrientationVertical 0
    let
        getViewState :: GCreateView sel edit -> View sel edit (ViewState sel edit ())
        getViewState gview =
            viewCreateView $ do
                widget <- traceBracket "GTK.Switch:getViewState.gview" $ gview
                lcContainPackStart True box widget
                #show widget
    firstvs <-
        cvLiftView $ do
            firstspec <- viewMapEdit (readOnlyEditLens specfunc) $ viewObjectRead $ \_ mr -> mr ReadWhole
            getViewState firstspec
    unliftView <- cvLiftView askUnliftIO
    cvDynamic @(ViewState sel edit ()) firstvs $ \object edits -> traceBracket "GTK.Switch:update" $ do
        whedits <- liftIO $ objectMapUpdates specfunc object edits
        case lastWholeEdit whedits of
            Nothing -> return ()
            Just spec -> do
                oldvs <- get
                liftIO $ closeDynamicView oldvs
                newvs <- liftIO $ runTransform (traceThing "GTK.Switch:update.getViewState" unliftView) $ getViewState spec
                put newvs
    toWidget box

switchGetView :: GetGView
switchGetView =
    MkGetView $ \getview uispec -> do
        spec <- isUISpec uispec
        return $
            case spec of
                MkSwitchUISpec specfunc -> switchView $ funcEditFunction getview . specfunc
