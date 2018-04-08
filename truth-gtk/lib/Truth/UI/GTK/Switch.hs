module Truth.UI.GTK.Switch
    ( switchGetView
    ) where

import GI.Gtk hiding (get)
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

switchView ::
       forall edit. (UISpec edit -> GCreateView edit) -> EditFunction edit (WholeEdit (UISpec edit)) -> GCreateView edit
switchView getview specfunc = do
    box <- liftIO $ boxNew OrientationVertical 0
    let
        getViewState :: UISpec edit -> View edit (ViewState edit ())
        getViewState spec =
            viewCreateView $ do
                widget <- getview spec
                lcContainPackStart True box widget
                #show widget
    firstvs <-
        cvLiftView $ do
            firstspec <- mapViewEdit (readOnlyEditLens specfunc) $ viewObjectRead $ \_ mr -> mr ReadWhole
            getViewState firstspec
    cvDynamic @(ViewState edit ()) firstvs $ \(MkUnliftIO unliftIO) ->
        mapReceiveUpdatesT specfunc $ \_ edits ->
            case lastWholeEdit edits of
                Nothing -> return ()
                Just spec -> do
                    oldvs <- get
                    liftIO $ closeDynamicView oldvs
                    newvs <- liftIO $ unliftIO $ getViewState spec
                    put newvs
    toWidget box

switchGetView :: GetGView
switchGetView =
    MkGetView $ \getview uispec -> do
        spec <- isUISpec uispec
        return $
            case spec of
                MkUISwitch specfunc -> switchView getview specfunc
