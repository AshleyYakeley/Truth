module Changes.World.GNOME.GTK.Widget.WebView
    ( WebViewOptions (..)
    , defaultWebViewOptions
    , createWebView
    )
where

import Network.URI (URI)

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

data WebViewOptions = MkWebViewOptions
    { wvoURISchemes :: [(Text, Text -> URI -> GView 'Locked (Maybe Media))]
    }

defaultWebViewOptions :: WebViewOptions
defaultWebViewOptions =
    MkWebViewOptions
        { wvoURISchemes = []
        }

createWebView :: WebViewOptions -> Model (ROWUpdate Text) -> GView 'Unlocked GI.Widget
createWebView MkWebViewOptions{..} lmod = do
    (wv, widget) <-
        gvRunLocked $ do
            let _ = wvoURISchemes
            -- The default network session is released by an exit handler on the
            -- process main thread, but WebKit requires its GTK thread for cleanup.
            -- Own the session here so it is released within the GTK lifecycle.
            session <- GI.networkSessionNew Nothing Nothing
            gvBind session
            webView <- gvNew GI.WebView [#networkSession GI.:= session]
            widget <- GI.toWidget webView
            return (webView, widget)
    gvBindReadOnlyWholeModel lmod $ \text -> gvRunLocked $ GI.webViewLoadHtml wv text Nothing
    return widget
