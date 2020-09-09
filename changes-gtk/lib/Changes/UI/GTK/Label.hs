module Changes.UI.GTK.Label
    ( createLabel
    ) where

import Changes.Core
import Changes.UI.GTK.Useful
import GI.Gtk
import Shapes

createLabel :: Model (ROWUpdate Text) -> CreateView Widget
createLabel lmod = do
    widget <- cvNew Label []
    cvBindReadOnlyWholeModel lmod $ \label -> set widget [#label := label]
    toWidget widget
