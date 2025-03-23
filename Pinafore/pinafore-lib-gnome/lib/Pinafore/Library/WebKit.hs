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
type WebViewOptionsSig = '[[(Text, Text -> URI -> Action Media)]]

webViewType :: ListType QDocSignature WebViewOptionsSig
webViewType =
    ConsListType
        (mkValueDocSignature "uriSchemes" "" $ Just [])
        NilListType

wvOptions :: ListProduct WebViewOptionsSig -> WebViewOptions
wvOptions (uriSchemes, ()) =
    defaultWebViewOptions
        { wvoURISchemes = (fmap $ fmap $ fmap $ fmap $ fmap knowToMaybe . gvRunUnlocked . gvLiftView . unliftAction) uriSchemes
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
