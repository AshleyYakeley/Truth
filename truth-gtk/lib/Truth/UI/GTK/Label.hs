module Truth.UI.GTK.Label
    ( labelGetView
    ) where

import GI.Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.Debug.Object

createWidget :: UILabel seledit edit -> CreateView seledit edit Widget
createWidget MkUILabel = traceBracket "labelGetView:createWidget" $ do
    widget <- new Label []
    traceBracket "labelGetView:createWidget.bind" $ cvBindEditFunction id $ \label -> traceBracket "labelGetView:set" $ set widget [#label := label]
    toWidget widget

labelGetView :: GetGView
labelGetView = MkGetView $ \_ uispec -> fmap createWidget $ isUISpec uispec
