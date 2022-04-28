module Changes.UI.GTK.Blank
    ( createBlank
    ) where

import Changes.Core
import Changes.GI
import GI.Gtk

createBlank :: View Widget
createBlank = do
    widget <- cvNew DrawingArea []
    toWidget widget
