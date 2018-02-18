module Truth.UI.GTK.Label
    ( labelGetView
    ) where

import GI.Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView

createWidget :: UILabel edit -> CreateView edit Widget
createWidget MkUILabel = do
    widget <- new Label []
    createViewBindEditFunction id $ \label -> set widget [#label := label]
    toWidget widget

labelGetView :: GetGView
labelGetView = MkGetView $ \_ uispec -> fmap createWidget $ isUISpec uispec
