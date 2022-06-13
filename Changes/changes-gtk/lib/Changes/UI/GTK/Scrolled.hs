module Changes.UI.GTK.Scrolled
    ( createScrolled
    ) where

import Changes.GI
import GI.Gtk
import Shapes

createScrolled :: Widget -> GView 'Locked Widget
createScrolled content = do
    sw <- gvNew ScrolledWindow []
    scrollable <- liftIO $ isScrollable content
    if scrollable
        then #add sw content
        else do
            viewport <- gvNew Viewport []
            #add viewport content
            #add sw viewport
    toWidget sw
