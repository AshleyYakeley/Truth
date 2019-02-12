module Truth.UI.GTK.Button
    ( buttonGetView
    ) where

import GI.Gdk
import GI.Gtk as Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful
import Truth.Debug.Object

createWidget :: EditFunction edit (WholeEdit Text) -> IO () -> CreateView sel edit Widget
createWidget label action = do
    widget <- new Button []
    traceBracket "GTK.Button:create.bind" $ cvBindEditFunction label $ \val -> set widget [#label := val]
    _ <- cvLiftView $ viewOn widget #clicked $ traceBracket "GTK.Button:click" $ liftIO action
    toWidget widget

buttonGetView :: GetGView
buttonGetView =
    MkGetView $ \_ uispec -> do
        MkUIButton label action <- isUISpec uispec
        return $ createWidget label action
