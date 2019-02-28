module Truth.UI.GTK.Scrolled
    ( scrolledGetView
    ) where

import GI.Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

scrolledGetView :: GetGView
scrolledGetView =
    MkGetView $ \getview uispec -> do
        MkScrolledUISpec cspec <- isUISpec uispec
        return $ do
            content <- getview cspec
            sw <- new ScrolledWindow []
            scrollable <- liftIO $ isScrollable content
            if scrollable
                then #add sw content
                else do
                    viewport <- new Viewport []
                    #add viewport content
                    #add sw viewport
            toWidget sw
