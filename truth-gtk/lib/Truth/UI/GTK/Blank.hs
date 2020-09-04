module Truth.UI.GTK.Blank
    ( createBlank
    ) where

import GI.Gtk
import Truth.Core
import Truth.UI.GTK.Useful

createBlank :: CreateView Widget
createBlank = do
    widget <- cvNew DrawingArea []
    toWidget widget
