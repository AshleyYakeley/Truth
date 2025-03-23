module Changes.World.GNOME.GTK.Widget.WebView
    ( WebViewOptions (..)
    , defaultWebViewOptions
    , createWebView
    )
where

import Changes.Core
import GI.Gio
import GI.Gtk
import GI.WebKit2
import Network.URI
import Shapes

import Changes.World.GNOME.GI

data WebViewOptions = MkWebViewOptions
    { wvoURISchemes :: [(Text, Text -> URI -> IO (Maybe Media))]
    }

defaultWebViewOptions :: WebViewOptions
defaultWebViewOptions =
    MkWebViewOptions
        { wvoURISchemes = []
        }

createWebView :: WebViewOptions -> Model (ROWUpdate Text) -> GView 'Unlocked Widget
createWebView MkWebViewOptions{..} lmod = do
    (wv, widget) <-
        gvRunLocked $ do
            webContext <- webContextNewEphemeral
            gvAcquire webContext
            for_ wvoURISchemes $ \(name, call) -> webContextRegisterUriScheme webContext name $ \request -> do
                method <- uRISchemeRequestGetHttpMethod request
                uriText <- uRISchemeRequestGetUri request
                case parseURIReference (unpack uriText) of
                    Just uri -> do
                        mMedia <- call method uri
                        for_ mMedia $ \(MkMedia mt bs) -> do
                            stream <- memoryInputStreamNewFromData bs Nothing
                            uRISchemeRequestFinish request stream (fromIntegral $ olength bs) $ Just $ encode textMediaTypeCodec mt
                    Nothing -> do
                        err <- gerrorNew 0 0 $ "bad URI: " <> uriText
                        uRISchemeRequestFinishError request err
            webView <- webViewNewWithContext webContext
            gvAcquire webView
            widget <- toWidget webView
            return (webView, widget)
    gvBindReadOnlyWholeModel lmod $ \text -> gvRunLocked $ webViewLoadHtml wv text Nothing
    return widget
