module Changes.World.GNOME.GTK.Element.Switch
    ( createDynamic
    ) where

import Changes.Core
import Changes.World.GNOME.GI
import GI.Gtk hiding (get)
import Shapes

createDynamic :: Model (ROWUpdate (GView 'Locked Widget)) -> GView 'Locked Widget
createDynamic model = do
    box <- liftIO $ boxNew OrientationVertical 0
    let
        addWidget :: GView 'Locked Widget -> GView 'Unlocked ()
        addWidget cvw =
            gvRunLocked $ do
                widget <- cvw
                gvPackStart True box widget
                #showAll widget
    gvRunUnlocked $ gvSwitch $ mapModel (funcChangeLens addWidget) model
    toWidget box
