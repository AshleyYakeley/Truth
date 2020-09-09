module Changes.UI.GTK.Blank
    ( createBlank
    ) where

import Changes.Core
import Changes.UI.GTK.Useful
import GI.Gtk

createBlank :: CreateView Widget
createBlank = do
    widget <- cvNew DrawingArea []
    toWidget widget
