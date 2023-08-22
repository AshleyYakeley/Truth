module Changes.World.GNOME.GTK.Widget.Label
    ( createLabel
    ) where

import Changes.Core
import Changes.World.GNOME.GI
import GI.Gtk
import Shapes

createLabel :: Model (ROWUpdate Text) -> GView 'Unlocked Widget
createLabel lmod = do
    (label, widget) <-
        gvRunLocked $ do
            label <- gvNew Label []
            widget <- toWidget label
            return (label, widget)
    gvBindReadOnlyWholeModel lmod $ \text -> gvRunLocked $ set label [#label := text]
    return widget
