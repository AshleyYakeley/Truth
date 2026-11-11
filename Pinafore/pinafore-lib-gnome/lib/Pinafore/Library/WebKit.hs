module Pinafore.Library.WebKit
    ( webKitStuff
    )
where

import Changes.Core
import Changes.World.GNOME.GTK
import Data.Shim ()
import Network.URI
import Pinafore.API
import Pinafore.Library.Media
import Shapes

import Pinafore.Library.GTK.Widget

type WebViewOptionsSig :: [Type]
type WebViewOptionsSig = '[URI -> Maybe (Action ()), Maybe CSSText]

webViewType :: ListType QDocSignature WebViewOptionsSig
webViewType =
    ConsListType (mkValueDocSignature "onLinkClicked" "" $ Just $ \_ -> Nothing)
        $ ConsListType (mkValueDocSignature "styleSheet" "" $ Just Nothing)
        $ NilListType

wvOptions :: ListProduct WebViewOptionsSig -> WebViewOptions
wvOptions (uriSchemes, (mStyleSheet, ())) =
    defaultWebViewOptions
        { wvoOnLinkClicked = \linkText -> do
            linkURI <- parseURIReference $ unpack linkText
            action <- uriSchemes linkURI
            return $ gvLiftView $ runAction action
        , wvoStyleSheet = fmap unCSSText mStyleSheet
        }

webViewVal :: ListProduct WebViewOptionsSig -> ImmutableWholeModel HTMLText -> LangWidget
webViewVal opts model =
    MkLangWidget $ \_ -> createWebView (wvOptions opts) $ unWModel $ immutableWholeModelValue mempty $ fmap unHTMLText model

webKitStuff :: LibraryStuff
webKitStuff =
    headingBDS "WebKit" "WebKit HTML rendering, etc."
        $ pure
        $ namespaceBDS
            "WebKit"
            [ recordValueBDS "webView" "" webViewType webViewVal
            ]
