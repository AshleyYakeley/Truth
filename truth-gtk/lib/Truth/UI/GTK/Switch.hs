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
        getViewState :: GCreateView sel edit -> View sel edit (ViewState sel)
        getViewState gview =
            viewCreateView $ do
                widget <- traceBracket "GTK.Switch:getViewState.gview" $ gview
                lcContainPackStart True box widget
                #show widget
    firstvs <- do
        firstspec <- cvMapEdit (readOnlyEditLens specfunc) $ cvLiftView $ viewObjectRead $ \_ mr -> mr ReadWhole
        cvLiftView $ getViewState firstspec
    unliftView <- cvLiftView askUnliftIO
    cvDynamic @(ViewState sel) firstvs $ \object edits -> traceBracket "GTK.Switch:update" $ do
        whedits <- liftIO $ objectMapUpdates specfunc object edits
        for_ (lastWholeEdit whedits) $ \spec -> do
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
