module Truth.UI.GTK.Layout
    ( Orientation(..)
    , createLayout
    ) where

import GI.Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.Useful

createLayout :: Orientation -> [(Bool, Widget)] -> CreateView Widget
createLayout orientation contents = do
    box <- cvNew Box [#orientation := orientation]
    for_ contents $ \(grow, widget) -> #packStart box widget grow grow 0
    toWidget box
