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
            webView <- gvNew GI.WebView []
            gvBind webView
            widget <- GI.toWidget webView
            return (webView, widget)
    gvBindReadOnlyWholeModel lmod $ \text -> gvRunLocked $ GI.webViewLoadHtml wv text Nothing
    return widget
