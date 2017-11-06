module Truth.UI.GTK.GView where

import Graphics.UI.Gtk
import Shapes
import Truth.Core

makeButton :: String -> IO () -> IO Button
makeButton name action = do
    button <- buttonNew
    set button [buttonLabel := name]
    _ <- on button buttonActivated action
    return button

type GCreateView edit = CreateView edit Widget

type GViewResult edit = ViewResult edit Widget

type GetGView = GetView Widget
