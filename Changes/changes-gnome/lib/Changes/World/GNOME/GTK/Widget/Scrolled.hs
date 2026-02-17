module Changes.World.GNOME.GTK.Widget.Scrolled
    ( createScrolled
    )
where

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

createScrolled :: GI.Widget -> GView 'Unlocked GI.Widget
createScrolled content =
    gvRunLocked $ do
        sw <- gvNew GI.ScrolledWindow []
        scrollable <- liftIO $ isScrollable content
        if scrollable
            then #setChild sw $ Just content
            else do
                viewport <- gvNew GI.Viewport []
                #setChild viewport $ Just content
                #setChild sw $ Just viewport
        GI.toWidget sw
