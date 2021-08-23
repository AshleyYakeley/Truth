module Changes.UI.GTK.Notebook
    ( createNotebook
    ) where

import Changes.Core
import Changes.UI.GTK.Useful
import GI.Gtk
import Shapes

createNotebook :: SelectNotify Int -> [(Widget, Widget)] -> CreateView Widget
createNotebook notifier pages = do
    notebook <- cvNew Notebook []
    for_ pages $ \(headwidget, bodywidget) -> #appendPage notebook bodywidget $ Just headwidget
    _ <- cvOn notebook #switchPage $ \_ i -> runSelectNotify notifier $ return $ Just $ fromIntegral i
    toWidget notebook
