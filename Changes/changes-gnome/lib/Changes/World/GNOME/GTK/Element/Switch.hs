module Changes.World.GNOME.GTK.Element.Switch
    ( createDynamic
    ) where

import Changes.Core
import Changes.World.GNOME.GI
import GI.Gtk hiding (get)
import Shapes

createDynamic :: Model (ROWUpdate (GView 'Unlocked Widget)) -> GView 'Unlocked Widget
createDynamic model =
    gvRunLockedThen $ do
        box <- boxNew OrientationVertical 0
        widget <- toWidget box
        let
            addItem :: GView 'Unlocked Widget -> GView 'Unlocked ()
            addItem cvw = do
                item <- cvw
                gvRunLocked $ do
                    gvPackStart True box item
                    #showAll item
        return $ do
            gvSwitch $ mapModel (funcChangeLens addItem) model
            return widget
