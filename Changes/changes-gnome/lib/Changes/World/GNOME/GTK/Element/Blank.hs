module Changes.World.GNOME.GTK.Element.Blank
    ( createBlank
    ) where

import Changes.World.GNOME.GI
import GI.Gtk
import Shapes

createBlank :: GView 'Unlocked Widget
createBlank =
    gvRunLocked $ do
        widget <- gvNew DrawingArea []
        toWidget widget
