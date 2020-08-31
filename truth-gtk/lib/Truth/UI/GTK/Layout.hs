module Truth.UI.GTK.Layout
    ( Orientation(..)
    , createLayout
    ) where

import GI.Gtk
import Shapes
import Truth.Core

createLayout :: Orientation -> [(Bool, Widget)] -> CreateView Widget
createLayout orientation contents = do
    box <- new Box [#orientation := orientation]
    for_ contents $ \(grow, widget) -> #packStart box widget grow grow 0
    toWidget box
