module Changes.World.GNOME.GTK.Widget.Label
    ( createLabel
    )
where

import Changes.Core
import GI.Gtk
import Shapes

import Changes.World.GNOME.GI

createLabel :: Model (ROWUpdate Text) -> GView 'Unlocked Widget
createLabel lmod = do
    (label, widget) <-
        gvRunLocked $ do
            label <- gvNew Label []
            widget <- toWidget label
            return (label, widget)
    gvBindReadOnlyWholeModel lmod $ \text -> gvRunLocked $ set label [#label := text]
    return widget
