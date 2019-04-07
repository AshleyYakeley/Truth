module Truth.UI.GTK.Label
    ( labelGetView
    ) where

import GI.Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView

createWidget :: LabelUISpec sel edit -> CreateView sel edit Widget
createWidget MkLabelUISpec = do
    widget <- new Label []
    cvBindEditFunction Nothing id $ \label -> set widget [#label := label]
    toWidget widget

labelGetView :: GetGView
labelGetView = MkGetView $ \_ uispec -> fmap createWidget $ isUISpec uispec
