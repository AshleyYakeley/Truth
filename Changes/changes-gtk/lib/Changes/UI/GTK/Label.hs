module Changes.UI.GTK.Label
    ( createLabel
    ) where

import Changes.Core
import Changes.GI
import GI.Gtk
import Shapes
import Changes.Debug.Reference

createLabel :: Model (ROWUpdate Text) -> View Widget
createLabel lmod = traceBracket "GTK.Label:create" $ do
    widget <- cvNew Label []
    traceBracket "GTK.Label:create.bind" $ viewBindReadOnlyWholeModel lmod $ \label -> traceBracket "GTK.Label:set" $ set widget [#label := label]
    toWidget widget
