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
type WebViewOptionsSig = '[URI -> Maybe (Action ())]

webViewType :: ListType QDocSignature WebViewOptionsSig
webViewType =
    ConsListType
        (mkValueDocSignature "onLinkClicked" "" $ Just $ \_ -> Nothing)
        NilListType

wvOptions :: ListProduct WebViewOptionsSig -> WebViewOptions
wvOptions (uriSchemes, ()) =
    defaultWebViewOptions
        { wvoOnLinkClicked = \linkText -> do
            linkURI <- parseURIReference $ unpack linkText
            action <- uriSchemes linkURI
            return $ gvLiftView $ runAction action
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
