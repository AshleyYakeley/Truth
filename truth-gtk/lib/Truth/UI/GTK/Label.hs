module Truth.UI.GTK.Label
    ( createLabel
    ) where

import GI.Gtk
import Shapes
import Truth.Core

createLabel :: Model (ROWUpdate Text) -> CreateView Widget
createLabel lmod = do
    widget <- new Label []
    cvBindReadOnlyWholeModel lmod $ \label -> set widget [#label := label]
    toWidget widget
