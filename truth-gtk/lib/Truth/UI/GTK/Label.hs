module Truth.UI.GTK.Label
    ( labelGetView
    ) where

import GI.Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.Debug.Object

createWidget :: LabelUISpec -> CreateView Widget
createWidget (MkLabelUISpec sub) = traceBracket "GTK.Label:create" $ do
    widget <- new Label []
    traceBracket "GTK.Label:create.bind" $ cvBindReadOnlyWholeModel sub $ \label -> traceBracket "GTK.Label:set" $ set widget [#label := label]
    toWidget widget

labelGetView :: GetGView
labelGetView = MkGetView $ \_ uispec -> fmap createWidget $ isUISpec uispec
