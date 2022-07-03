module Changes.UI.GTK.Element.Button
    ( createButton
    ) where

import Changes.Core
import Changes.Debug
import Changes.GI
import Data.IORef
import GI.Gtk
import Shapes

createButton :: Model (ROWUpdate Text) -> Model (ROWUpdate (Maybe (GView 'Locked ()))) -> GView 'Locked Widget
createButton rlabel raction = do
    aref <- liftIO $ newIORef Nothing
    widget <- gvNew Button []
    gvBindReadOnlyWholeModel rlabel $ \label -> gvLiftIO $ set widget [#label := label]
    gvBindReadOnlyWholeModel raction $ \maction -> do
        gvLiftIONoUI $ writeIORef aref maction
        gvRunLocked $ set widget [#sensitive := isJust maction]
    _ <-
        gvOnSignal widget #clicked $
        traceBracket "createButton.clicked" $ do
            maction <- liftIO $ readIORef aref
            for_ maction id
    toWidget widget
