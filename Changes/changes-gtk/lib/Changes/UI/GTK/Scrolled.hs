module Changes.UI.GTK.Scrolled
    ( createScrolled
    ) where

import Changes.Core
import Changes.UI.GTK.Useful
import GI.Gtk
import Shapes

createScrolled :: Widget -> CreateView Widget
createScrolled content = do
    sw <- cvNew ScrolledWindow []
    scrollable <- liftIO $ isScrollable content
    if scrollable
        then #add sw content
        else do
            viewport <- cvNew Viewport []
            #add viewport content
            #add sw viewport
    toWidget sw
