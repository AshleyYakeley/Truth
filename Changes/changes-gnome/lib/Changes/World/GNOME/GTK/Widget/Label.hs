module Changes.World.GNOME.GTK.Widget.Label
    ( createLabel
    )
where

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

createLabel :: Model (ROWUpdate Text) -> GView 'Unlocked GI.Widget
createLabel lmod = do
    (label, widget) <-
        gvRunLocked $ do
            label <- gvNew GI.Label []
            widget <- GI.toWidget label
            return (label, widget)
    gvBindReadOnlyWholeModel lmod $ \text -> gvRunLocked $ GI.set label [#label GI.:= text]
    return widget
