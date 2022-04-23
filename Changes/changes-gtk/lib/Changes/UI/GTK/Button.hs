module Changes.UI.GTK.Button
    ( createButton
    ) where

import Changes.Core
import Changes.GI
import Data.IORef
import GI.Gtk
import Shapes

createButton :: Model (ROWUpdate Text) -> Model (ROWUpdate (Maybe (View ()))) -> View Widget
createButton rlabel raction = do
    aref <- liftIO $ newIORef Nothing
    widget <- cvNew Button []
    viewBindReadOnlyWholeModel rlabel $ \label -> set widget [#label := label]
    viewBindReadOnlyWholeModel raction $ \maction -> do
        liftIO $ writeIORef aref maction
        set widget [#sensitive := isJust maction]
    _ <-
        viewOn widget #clicked $ do
            maction <- liftIO $ readIORef aref
            case maction of
                Nothing -> return ()
                Just action -> action
    toWidget widget
