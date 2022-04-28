module Changes.UI.GTK.Notebook
    ( createNotebook
    ) where

import Changes.Core
import Changes.GI
import GI.Gtk
import Shapes

createNotebook :: SelectNotify Int -> [(Widget, Widget)] -> View Widget
createNotebook notifier pages = do
    notebook <- cvNew Notebook []
    for_ pages $ \(headwidget, bodywidget) -> #appendPage notebook bodywidget $ Just headwidget
    _ <- viewOn notebook #switchPage $ \_ i -> runSelectNotify notifier $ return $ Just $ fromIntegral i
    toWidget notebook
