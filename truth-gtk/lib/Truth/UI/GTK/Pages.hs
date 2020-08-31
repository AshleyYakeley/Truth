module Truth.UI.GTK.Pages
    ( createNotebook
    ) where

import GI.Gtk
import Shapes
import Truth.Core

createNotebook :: [(Widget, Widget)] -> CreateView Widget
createNotebook pages = do
    notebook <- new Notebook []
    for_ pages $ \(headwidget, bodywidget) -> #appendPage notebook bodywidget $ Just headwidget
    toWidget notebook
