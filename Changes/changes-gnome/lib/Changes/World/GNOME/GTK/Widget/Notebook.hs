module Changes.World.GNOME.GTK.Widget.Notebook
    ( createNotebook
    )
where

import Changes.Core
import GI.Gtk
import Shapes

import Changes.World.GNOME.GI

createNotebook :: SelectNotify Int -> [(Widget, Widget)] -> GView 'Unlocked Widget
createNotebook notifier pages =
    gvRunLocked $ do
        (notebook, widget) <- gvNewWidget Notebook []
        for_ pages $ \(headwidget, bodywidget) -> #appendPage notebook bodywidget $ Just headwidget
        _ <-
            gvOnSignal notebook #switchPage $ \_ i ->
                gvRunUnlocked $ gvLiftView $ runSelectNotify notifier $ return $ Just $ fromIntegral i
        return widget
