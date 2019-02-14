module Truth.UI.GTK.Switch
    ( switchGetView
    ) where

import GI.Gtk hiding (get)
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

switchView ::
       forall sel edit.
       (UISpec sel edit -> GCreateView sel edit)
    -> EditFunction edit (WholeEdit (UISpec sel edit))
    -> GCreateView sel edit
switchView getview specfunc = do
    box <- liftIO $ boxNew OrientationVertical 0
    let
        getViewState :: UISpec sel edit -> View sel edit (ViewState sel edit ())
        getViewState spec =
            viewCreateView $ do
                widget <- getview spec
                lcContainPackStart True box widget
                #show widget
    firstvs <-
        cvLiftView $ do
            firstspec <- viewMapEdit (readOnlyEditLens specfunc) $ viewObjectRead $ \_ mr -> mr ReadWhole
            getViewState firstspec
    unliftView <- cvLiftView askUnliftIO
    cvDynamic @(ViewState sel edit ()) firstvs $ \object edits -> do
        whedits <- liftIO $ objectMapUpdates specfunc object edits
        case lastWholeEdit whedits of
            Nothing -> return ()
            Just spec -> do
                oldvs <- get
                liftIO $ closeDynamicView oldvs
                newvs <- liftIO $ runTransform unliftView $ getViewState spec
                put newvs
    toWidget box

switchGetView :: GetGView
switchGetView =
    MkGetView $ \getview uispec -> do
        spec <- isUISpec uispec
        return $
            case spec of
                MkSwitchUISpec specfunc -> switchView getview specfunc
