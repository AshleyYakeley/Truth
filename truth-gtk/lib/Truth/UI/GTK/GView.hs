module Truth.UI.GTK.GView where

import GI.Gtk
import Shapes
import Truth.Core

makeButton :: Text -> IO () -> IO Button
makeButton name action = do
    button <- new Button [#label := name]
    _ <- on button #clicked action
    return button

type GCreateView edit = CreateView edit Widget

type GViewResult edit = ViewResult edit Widget

type GetGView = GetView Widget
