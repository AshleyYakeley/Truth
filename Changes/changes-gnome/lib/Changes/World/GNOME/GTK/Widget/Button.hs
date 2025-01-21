module Changes.World.GNOME.GTK.Widget.Button
    ( createButton
    )
where

import Changes.Core
import Data.IORef
import GI.Gtk
import Shapes

import Changes.World.GNOME.GI

createButton :: Model (ROWUpdate Text) -> Model (ROWUpdate (Maybe (GView 'Locked ()))) -> GView 'Unlocked Widget
createButton rlabel raction = do
    aref <- gvLiftIONoUI $ newIORef Nothing
    gvRunLockedThen $ do
        (button, widget) <- gvNewWidget Button []
        _ <-
            gvOnSignal button #clicked $ do
                maction <- liftIO $ readIORef aref
                for_ maction id
        return $ do
            gvBindReadOnlyWholeModel rlabel $ \label -> gvRunLocked $ set button [#label := label]
            gvBindReadOnlyWholeModel raction $ \maction -> do
                gvLiftIONoUI $ writeIORef aref maction
                gvRunLocked $ set button [#sensitive := isJust maction]
            return widget
