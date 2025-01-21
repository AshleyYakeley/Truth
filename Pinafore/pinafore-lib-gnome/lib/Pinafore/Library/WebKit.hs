module Pinafore.Library.WebKit
    ( webKitStuff
    )
where

import Changes.Core
import Changes.World.GNOME.GTK
import Pinafore.API
import Pinafore.Library.Media
import Shapes

import Pinafore.Library.GTK.Widget

webViewWidget :: ImmutableWholeModel HTMLText -> LangWidget
webViewWidget model =
    MkLangWidget $ \_ -> createWebView $ unWModel $ immutableWholeModelValue mempty $ fmap unHTMLText model

webKitStuff :: LibraryStuff
webKitStuff =
    headingBDS "WebKit" "WebKit HTML rendering, etc." $ pure $ namespaceBDS "WebKit" [valBDS "webView" "" webViewWidget]
