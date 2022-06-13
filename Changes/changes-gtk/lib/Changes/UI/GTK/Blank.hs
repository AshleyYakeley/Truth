module Changes.UI.GTK.Blank
    ( createBlank
    ) where

import Changes.GI
import GI.Gtk

createBlank :: GView 'Locked Widget
createBlank = do
    widget <- gvNew DrawingArea []
    toWidget widget
