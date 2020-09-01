module Truth.UI.GTK.Label
    ( createLabel
    ) where

import GI.Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.Useful

createLabel :: Model (ROWUpdate Text) -> CreateView Widget
createLabel lmod = do
    widget <- cvNew Label []
    cvBindReadOnlyWholeModel lmod $ \label -> set widget [#label := label]
    toWidget widget
