module Changes.UI.GTK.CSS
    ( setCSSName
    , setCSSClass
    , setCSSStyleSheet
    , bindCSS
    ) where

import Changes.Core
import Changes.UI.GTK.Useful
import GI.Gtk as GI
import Shapes

setCSSName :: Text -> Widget -> CreateView ()
setCSSName name w = #setName w name

setCSSClass :: Text -> Widget -> CreateView ()
setCSSClass cssclass w = do
    sc <- #getStyleContext w
    #addClass sc cssclass

setCSSStyleSheet :: Bool -> Word32 -> Text -> Widget -> CreateView ()
setCSSStyleSheet full priority css w = do
    provider <- cvNew CssProvider []
    #loadFromData provider $ encodeUtf8 css
    children <- liftIO $ widgetGetTree full w
    for_ children $ \child -> do
        sc <- #getStyleContext child
        #addProvider sc provider priority

bindCSS :: Word32 -> Model (ROWUpdate Text) -> Widget -> CreateView ()
bindCSS priority cssmod widget = do
    provider <- cvNew CssProvider []
    sc <- #getStyleContext widget
    #addProvider sc provider priority
    cvBindReadOnlyWholeModel cssmod $ \css -> #loadFromData provider $ encodeUtf8 css
