module Changes.UI.GTK.Layout
    ( Orientation(..)
    , createLayout
    ) where

import Changes.Core
import Changes.UI.GTK.Useful
import GI.Gtk
import Shapes

createLayout :: Orientation -> [(Bool, Widget)] -> CreateView Widget
createLayout orientation contents = do
    box <- cvNew Box [#orientation := orientation]
    for_ contents $ \(grow, widget) -> #packStart box widget grow grow 0
    toWidget box
