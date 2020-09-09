module Changes.UI.GTK.Pages
    ( createNotebook
    ) where

import Changes.Core
import Changes.UI.GTK.Useful
import GI.Gtk
import Shapes

createNotebook :: [(Widget, Widget)] -> CreateView Widget
createNotebook pages = do
    notebook <- cvNew Notebook []
    for_ pages $ \(headwidget, bodywidget) -> #appendPage notebook bodywidget $ Just headwidget
    toWidget notebook
