module Truth.UI.GTK.Label
    ( labelGetView
    ) where

import GI.Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView

createWidget :: UILabel edit -> CreateView edit Widget
createWidget MkUILabel = do
    initial <- liftOuter $ viewObjectRead mutableReadToSubject
    widget <- new Label [#label := initial]
    createViewReceiveUpdate $ \_ (MkWholeEdit st) -> set widget [#label := st]
    toWidget widget

labelGetView :: GetGView
labelGetView = MkGetView $ \_ uispec -> fmap createWidget $ isUISpec uispec
