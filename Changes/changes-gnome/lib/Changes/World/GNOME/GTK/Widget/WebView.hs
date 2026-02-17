module Changes.World.GNOME.GTK.Widget.WebView
    ( WebViewOptions (..)
    , defaultWebViewOptions
    , createWebView
    )
where

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
            webContext <- webContextNewEphemeral
            gvAcquire webContext
            for_ wvoURISchemes $ \(name, call) ->
                gvWithUnliftLockedAsync $ \unlift -> webContextRegisterUriScheme webContext name $ \request -> do
                    method <- uRISchemeRequestGetHttpMethod request
                    uriText <- uRISchemeRequestGetUri request
                    case parseURIReference (unpack uriText) of
                        Just uri -> do
                            mMedia <- unlift $ call method uri
                            for_ mMedia $ \(MkMedia mt bs) -> do
                                stream <- memoryInputStreamNewFromData bs Nothing
                                uRISchemeRequestFinish request stream (fromIntegral $ olength bs) $ Just $ encode textMediaTypeCodec mt
                        Nothing -> do
                            err <- gerrorNew 0 0 $ "bad URI: " <> uriText
                            uRISchemeRequestFinishError request err
            webView <- GI.webViewNewWithContext webContext
            gvAcquire webView
            widget <- GI.toWidget webView
            return (webView, widget)
    gvBindReadOnlyWholeModel lmod $ \text -> gvRunLocked $ GI.webViewLoadHtml wv text Nothing
    return widget
