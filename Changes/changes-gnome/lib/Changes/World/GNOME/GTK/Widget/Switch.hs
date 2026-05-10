module Changes.World.GNOME.GTK.Widget.Switch
    ( createDynamic
    )
where

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

createDynamic :: Model (ROWUpdate (GView 'Unlocked GI.Widget)) -> GView 'Unlocked GI.Widget
createDynamic model =
    gvRunLockedThen $ do
        box <- GI.boxNew GI.OrientationVertical 0
        widget <- GI.toWidget box
        let
            addItem :: GView 'Unlocked GI.Widget -> GView 'Unlocked ()
            addItem cvw = do
                item <- cvw
                gvRunLocked $ gvBoxPrepend box item
        return $ do
            gvSwitch $ mapModel (funcChangeLens addItem) model
            return widget
