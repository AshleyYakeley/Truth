module Pinafore.Language.Library.WebKit
    ( webKitStuff
    ) where

import Changes.Core
import Changes.World.GNOME.GTK
import Pinafore.API
import Pinafore.Language.Library.GTK.Widget
import Shapes

webViewWidget :: ImmutableWholeModel Text -> LangWidget
webViewWidget model = MkLangWidget $ \_ -> createWebView $ unWModel $ immutableWholeModelValue mempty model

webKitStuff :: LibraryStuff ()
webKitStuff =
    headingBDS "WebKit" "WebKit HTML rendering, etc." $ pure $ namespaceBDS "WebKit" [valBDS "webView" "" webViewWidget]
