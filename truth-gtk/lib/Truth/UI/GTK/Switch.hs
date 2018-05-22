module Truth.UI.GTK.Switch
    ( switchGetView
    ) where

import GI.Gtk hiding (get)
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful
import Truth.Debug.Object

switchView ::
       forall seledit edit.
       (UISpec seledit edit -> GCreateView seledit edit)
    -> EditFunction edit (WholeEdit (UISpec seledit edit))
    -> GCreateView seledit edit
switchView getview specfunc = do
    box <- liftIO $ boxNew OrientationVertical 0
    let
        getViewState :: UISpec seledit edit -> View seledit edit (ViewState seledit edit ())
        getViewState spec =
            viewCreateView $ do
                widget <- traceBracket "switchView:getViewState.getview" $ getview spec
                lcContainPackStart True box widget
                #show widget
    firstvs <-
        cvLiftView $ do
            firstspec <- viewMapEdit (readOnlyEditLens specfunc) $ viewObjectRead $ \_ mr -> mr ReadWhole
            getViewState firstspec
    unliftView <- cvLiftView askUnliftIO
    cvDynamic @(ViewState seledit edit ()) firstvs $ \object edits -> traceBracket "switchView:update" $ do
        whedits <- liftIO $ objectMapUpdates specfunc object edits
        case lastWholeEdit whedits of
            Nothing -> return ()
            Just spec -> do
                oldvs <- get
                liftIO $ closeDynamicView oldvs
                newvs <- liftIO $ runUnliftIO (traceThing "switchView:update.getViewState" unliftView) $ getViewState spec
                put newvs
    toWidget box

switchGetView :: GetGView
switchGetView =
    MkGetView $ \getview uispec -> do
        spec <- isUISpec uispec
        return $
            case spec of
                MkUISwitch specfunc -> switchView getview specfunc
