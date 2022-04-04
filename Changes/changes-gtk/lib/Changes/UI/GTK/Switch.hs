module Changes.UI.GTK.Switch
    ( createDynamic
    ) where

import Changes.Core
import Changes.GI
import GI.Gtk hiding (get)
import Shapes
import Changes.Debug.Reference

createDynamic :: Model (ROWUpdate (CreateView Widget)) -> CreateView Widget
createDynamic model = do
    box <- liftIO $ boxNew OrientationVertical 0
    let
        addWidget :: CreateView Widget -> CreateView ()
        addWidget cvw = do
            widget <- traceBracket "GTK.Switch:addWidget.gview" cvw
            cvPackStart True box widget
            #showAll widget
    cvSwitch $ mapModel (funcChangeLens addWidget) model
    toWidget box
