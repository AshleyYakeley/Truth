module Changes.UI.GTK.Notebook
    ( createNotebook
    ) where

import Changes.Core
import Changes.UI.GTK.Useful
import GI.Gtk
import Shapes

createNotebook :: forall a. SelectNotify a -> [(Widget, Widget, a)] -> CreateView Widget
createNotebook notifier pages = do
    notebook <- cvNew Notebook []
    alist <-
        for pages $ \(headwidget, bodywidget, a) -> do
            _ <- #appendPage notebook bodywidget $ Just headwidget
            return a
    _ <- cvOn notebook #switchPage $ \_ i -> runSelectNotify notifier $ return $ index alist $ fromIntegral i
    toWidget notebook
