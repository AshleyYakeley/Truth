module Changes.World.GNOME.GTK.Widget.Notebook
    ( createNotebook
    )
where

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

createNotebook :: SelectNotify Int -> [(GI.Widget, GI.Widget)] -> GView 'Unlocked GI.Widget
createNotebook notifier pages =
    gvRunLocked $ do
        (notebook, widget) <- gvNewWidget GI.Notebook []
        for_ pages $ \(headwidget, bodywidget) -> #appendPage notebook bodywidget $ Just headwidget
        _ <-
            gvOnSignal notebook #switchPage $ \_ i ->
                gvRunUnlocked $ gvLiftView $ runSelectNotify notifier $ return $ Just $ fromIntegral i
        return widget
