module Truth.UI.GTK.Label
    ( labelGetView
    ) where

import GI.Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.Debug.Object

createWidget :: LabelUISpec sel edit -> CreateView sel edit Widget
createWidget MkLabelUISpec = traceBracket "GTK.Label:create" $ do
    widget <- new Label []
    traceBracket "GTK.Label:create.bind" $ cvBindUpdateFunction Nothing id $ \label -> traceBracket "GTK.Label:set" $ set widget [#label := label]
    toWidget widget

labelGetView :: GetGView
labelGetView = MkGetView $ \_ uispec -> fmap createWidget $ isUISpec uispec
