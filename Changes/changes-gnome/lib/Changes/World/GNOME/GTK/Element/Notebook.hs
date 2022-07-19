module Changes.World.GNOME.GTK.Element.Notebook
    ( createNotebook
    ) where

import Changes.Core
import Changes.World.GNOME.GI
import GI.Gtk
import Shapes

createNotebook :: SelectNotify Int -> [(Widget, Widget)] -> GView 'Locked Widget
createNotebook notifier pages = do
    notebook <- gvNew Notebook []
    for_ pages $ \(headwidget, bodywidget) -> #appendPage notebook bodywidget $ Just headwidget
    _ <-
        gvOnSignal notebook #switchPage $ \_ i ->
            gvRunUnlocked $ gvLiftView $ runSelectNotify notifier $ return $ Just $ fromIntegral i
    toWidget notebook
