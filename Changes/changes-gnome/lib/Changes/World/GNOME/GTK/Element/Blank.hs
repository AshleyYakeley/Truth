module Changes.World.GNOME.GTK.Element.Blank
    ( createBlank
    ) where

import Changes.World.GNOME.GI
import GI.Gtk

createBlank :: GView 'Locked Widget
createBlank = do
    widget <- gvNew DrawingArea []
    toWidget widget
