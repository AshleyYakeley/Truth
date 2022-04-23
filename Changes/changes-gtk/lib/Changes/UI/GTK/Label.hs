module Changes.UI.GTK.Label
    ( createLabel
    ) where

import Changes.Core
import Changes.GI
import GI.Gtk
import Shapes

createLabel :: Model (ROWUpdate Text) -> View Widget
createLabel lmod = do
    widget <- cvNew Label []
    viewBindReadOnlyWholeModel lmod $ \label -> set widget [#label := label]
    toWidget widget
