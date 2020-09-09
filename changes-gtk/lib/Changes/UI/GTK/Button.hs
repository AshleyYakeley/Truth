module Truth.UI.GTK.Button
    ( createButton
    ) where

import Data.IORef
import GI.Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.Useful

createButton :: Model (ROWUpdate Text) -> Model (ROWUpdate (Maybe (View ()))) -> CreateView Widget
createButton rlabel raction = do
    aref <- liftIO $ newIORef Nothing
    widget <- cvNew Button []
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
