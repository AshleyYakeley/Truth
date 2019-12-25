module Truth.UI.GTK.Label
    ( labelGetView
    ) where

import GI.Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView

createWidget :: LabelUISpec sel -> CreateView sel Widget
createWidget (MkLabelUISpec sub) = do
    widget <- new Label []
    cvBindReadOnlyWholeSubscriber sub $ \label -> set widget [#label := label]
    toWidget widget

labelGetView :: GetGView
labelGetView = MkGetView $ \_ uispec -> fmap createWidget $ isUISpec uispec
