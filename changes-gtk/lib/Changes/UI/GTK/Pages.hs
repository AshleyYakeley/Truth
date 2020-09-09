module Truth.UI.GTK.Pages
    ( createNotebook
    ) where

import GI.Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.Useful

createNotebook :: [(Widget, Widget)] -> CreateView Widget
createNotebook pages = do
    notebook <- cvNew Notebook []
    for_ pages $ \(headwidget, bodywidget) -> #appendPage notebook bodywidget $ Just headwidget
    toWidget notebook
