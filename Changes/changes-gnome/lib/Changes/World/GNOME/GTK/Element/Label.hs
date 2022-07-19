module Changes.World.GNOME.GTK.Element.Label
    ( createLabel
    ) where

import Changes.Core
import Changes.World.GNOME.GI
import GI.Gtk
import Shapes

createLabel :: Model (ROWUpdate Text) -> GView 'Locked Widget
createLabel lmod = do
    widget <- gvNew Label []
    gvBindReadOnlyWholeModel lmod $ \label -> gvLiftIO $ set widget [#label := label]
    toWidget widget
