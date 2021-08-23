module Changes.UI.GTK.Button
    ( createButton
    ) where

import Changes.Core
import Changes.UI.GTK.Useful
import Data.IORef
import GI.Gtk
import Shapes

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
