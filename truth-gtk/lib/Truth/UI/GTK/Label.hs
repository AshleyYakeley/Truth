module Truth.UI.GTK.Label
    ( createLabel
    ) where

import GI.Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.Useful
import Truth.Debug.Reference

createLabel :: Model (ROWUpdate Text) -> CreateView Widget
createLabel lmod = traceBracket "GTK.Label:create" $ do
    widget <- cvNew Label []
    traceBracket "GTK.Label:create.bind" $ cvBindReadOnlyWholeModel lmod $ \label -> traceBracket "GTK.Label:set" $ set widget [#label := label]
    toWidget widget
