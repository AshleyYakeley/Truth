module Changes.World.GNOME.GTK.Widget.Button
    ( createButton
    )
where

import Data.IORef

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

createButton :: Model (ROWUpdate Text) -> Model (ROWUpdate (Maybe (GView 'Locked ()))) -> GView 'Unlocked GI.Widget
createButton rlabel raction = do
    aref <- gvLiftIONoUI $ newIORef Nothing
    gvRunLockedThen $ do
        (button, widget) <- gvNewWidget GI.Button []
        _ <-
            gvOnSignal button #clicked $ do
                maction <- liftIO $ readIORef aref
                for_ maction id
        return $ do
            gvBindReadOnlyWholeModel rlabel $ \label -> gvRunLocked $ GI.set button [#label GI.:= label]
            gvBindReadOnlyWholeModel raction $ \maction -> do
                gvLiftIONoUI $ writeIORef aref maction
                gvRunLocked $ GI.set button [#sensitive GI.:= isJust maction]
            return widget
