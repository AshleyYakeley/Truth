module Truth.UI.GTK.Button
    ( buttonGetView
    ) where

import GI.Gdk
import GI.Gtk as Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

createWidget :: UIButton edit -> CreateView edit Widget
createWidget (MkUIButton label action) = do
    widget <- new Button []
    cvBindEditFunction label $ \val -> set widget [#label := val]
    _ <- cvLiftView $ viewOn widget #clicked action
    toWidget widget

buttonGetView :: GetGView
buttonGetView = MkGetView $ \_ uispec -> fmap createWidget $ isUISpec uispec
