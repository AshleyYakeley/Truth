module Changes.World.GNOME.GTK.Widget.WebView
    ( createWebView
    )
where

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

createWebView :: Model (ROWUpdate Text) -> GView 'Unlocked GI.Widget
createWebView lmod = do
    (wv, widget) <-
        gvRunLocked $ do
            wv <- gvNew GI.WebView []
            widget <- GI.toWidget wv
            return (wv, widget)
    gvBindReadOnlyWholeModel lmod $ \text -> gvRunLocked $ GI.webViewLoadHtml wv text Nothing
    return widget
