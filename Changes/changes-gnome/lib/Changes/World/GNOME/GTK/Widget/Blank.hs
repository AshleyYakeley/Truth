module Changes.World.GNOME.GTK.Widget.Blank
    ( createBlank
    )
where

import GI.Gtk
import Shapes

import Changes.World.GNOME.GI

createBlank :: GView 'Unlocked Widget
createBlank =
    gvRunLocked $ do
        widget <- gvNew DrawingArea []
        toWidget widget
