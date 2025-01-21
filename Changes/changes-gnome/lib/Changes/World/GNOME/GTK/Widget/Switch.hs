module Changes.World.GNOME.GTK.Widget.Switch
    ( createDynamic
    )
where

import Changes.Core
import GI.Gtk hiding (get)
import Shapes

import Changes.World.GNOME.GI

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
