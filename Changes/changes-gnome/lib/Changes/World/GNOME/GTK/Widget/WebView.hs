module Changes.World.GNOME.GTK.Widget.WebView
    ( WebViewOptions (..)
    , defaultWebViewOptions
    , createWebView
    )
where

import GI.WebKit qualified as WK

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

interceptLinks :: GTKAsyncUnlift () -> WK.WebView -> (Text -> Maybe (GView 'Unlocked ())) -> IO ()
interceptLinks unlift webView handleLink =
    void $ WK.onWebViewDecidePolicy webView $ \decision kind ->
        let
            intercept = do
                mNav <- GI.castTo WK.NavigationPolicyDecision decision
                case mNav of
                    Nothing -> pure False
                    Just nav -> do
                        action <- WK.navigationPolicyDecisionGetNavigationAction nav
                        navType <- WK.navigationActionGetNavigationType action
                        case navType of
                            WK.NavigationTypeLinkClicked -> do
                                request <- WK.navigationActionGetRequest action
                                uri <- WK.uRIRequestGetUri request
                                case handleLink uri of
                                    Just iou -> do
                                        WK.policyDecisionIgnore decision
                                        unlift iou
                                        pure True
                                    Nothing -> pure False
                            _ -> pure False
            in case kind of
                WK.PolicyDecisionTypeNavigationAction -> intercept
                WK.PolicyDecisionTypeNewWindowAction -> intercept
                _ -> pure False

newtype WebViewOptions = MkWebViewOptions
    { wvoOnLinkClicked :: Text -> Maybe (GView 'Unlocked ())
    }

defaultWebViewOptions :: WebViewOptions
defaultWebViewOptions =
    MkWebViewOptions
        { wvoOnLinkClicked = \_ -> Nothing
        }

createWebView :: WebViewOptions -> Model (ROWUpdate Text) -> GView 'Unlocked GI.Widget
createWebView MkWebViewOptions{..} lmod = do
    (wv, widget) <-
        gvRunLocked $ gvWithAsyncUnlift () $ \unlift -> do
            -- The default network session is released by an exit handler on the
            -- process main thread, but WebKit requires its GTK thread for cleanup.
            -- Own the session here so it is released within the GTK lifecycle.
            session <- WK.networkSessionNew Nothing Nothing
            gvBind session
            webView <- gvNew WK.WebView [#networkSession GI.:= session]
            liftIO $ interceptLinks unlift webView wvoOnLinkClicked
            widget <- GI.toWidget webView
            return (webView, widget)
    gvBindReadOnlyWholeModel lmod $ \text -> gvRunLocked $ WK.webViewLoadHtml wv text Nothing
    return widget
