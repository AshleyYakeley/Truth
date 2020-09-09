module Truth.UI.GTK.Scrolled
    ( createScrolled
    ) where

import GI.Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.Useful

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
