module Changes.UI.GTK.Label
    ( createLabel
    ) where

import Changes.Core
import Changes.GI
import GI.Gtk
import Shapes

createLabel :: Model (ROWUpdate Text) -> GView 'Locked Widget
createLabel lmod = do
    widget <- gvNew Label []
    gvBindReadOnlyWholeModel lmod $ \label -> gvLiftIO $ set widget [#label := label]
    toWidget widget
