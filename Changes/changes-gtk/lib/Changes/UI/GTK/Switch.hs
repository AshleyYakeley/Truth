module Changes.UI.GTK.Switch
    ( createDynamic
    ) where

import Changes.Core
import Changes.GI
import GI.Gtk hiding (get)
import Shapes

createDynamic :: Model (ROWUpdate (View Widget)) -> View Widget
createDynamic model = do
    box <- liftIO $ boxNew OrientationVertical 0
    let
        addWidget :: View Widget -> View ()
        addWidget cvw = do
            widget <- cvw
            cvPackStart True box widget
            #showAll widget
    viewSwitch $ mapModel (funcChangeLens addWidget) model
    toWidget box
