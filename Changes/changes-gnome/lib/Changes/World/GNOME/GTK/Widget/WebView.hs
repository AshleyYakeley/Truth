module Changes.World.GNOME.GTK.Widget.WebView
    ( createWebView
    )
where

import Changes.Core
import GI.Gtk
import GI.WebKit2
import Shapes

import Changes.World.GNOME.GI

createWebView :: Model (ROWUpdate Text) -> GView 'Unlocked Widget
createWebView lmod = do
    (wv, widget) <-
        gvRunLocked $ do
            wv <- gvNew WebView []
            widget <- toWidget wv
            return (wv, widget)
    gvBindReadOnlyWholeModel lmod $ \text -> gvRunLocked $ webViewLoadHtml wv text Nothing
    return widget
