module Changes.World.GNOME.GTK.Widget.Scrolled
    ( createScrolled
    )
where

import GI.Gtk
import Shapes

import Changes.World.GNOME.GI

createScrolled :: Widget -> GView 'Unlocked Widget
createScrolled content =
    gvRunLocked $ do
        sw <- gvNew ScrolledWindow []
        scrollable <- liftIO $ isScrollable content
        if scrollable
            then #add sw content
            else do
                viewport <- gvNew Viewport []
                #add viewport content
                #add sw viewport
        toWidget sw
