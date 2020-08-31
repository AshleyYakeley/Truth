module Truth.UI.GTK.Blank
    ( createBlank
    ) where

import GI.Gtk
import Truth.Core

createBlank :: CreateView Widget
createBlank = do
    widget <- new DrawingArea []
    toWidget widget
