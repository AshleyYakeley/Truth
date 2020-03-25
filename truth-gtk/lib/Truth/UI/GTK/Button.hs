module Truth.UI.GTK.Button
    ( buttonGetView
    ) where

import Data.IORef
import GI.Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

createWidget :: Model (ROWUpdate Text) -> Model (ROWUpdate (Maybe (View ()))) -> CreateView Widget
createWidget rlabel raction = do
    aref <- liftIO $ newIORef Nothing
    widget <- new Button []
    cvBindReadOnlyWholeModel rlabel $ \label -> set widget [#label := label]
    cvBindReadOnlyWholeModel raction $ \maction -> do
        liftIO $ writeIORef aref maction
        set widget [#sensitive := isJust maction]
    _ <-
        cvOn widget #clicked $ do
            maction <- liftIO $ readIORef aref
            case maction of
                Nothing -> return ()
                Just action -> action
    toWidget widget

buttonGetView :: GetGView
buttonGetView =
    MkGetView $ \_ uispec -> do
        MkButtonUISpec rlabel raction <- isUISpec uispec
        return $ createWidget rlabel raction
