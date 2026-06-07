module Changes.World.GNOME.GTK.Widget.Blank
    ( createBlank
    )
where

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

createBlank :: GView 'Unlocked GI.Widget
createBlank =
    gvRunLocked $ do
        widget <- gvNew GI.DrawingArea []
        GI.toWidget widget
