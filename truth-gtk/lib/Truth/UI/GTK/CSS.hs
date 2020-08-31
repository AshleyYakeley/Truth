module Truth.UI.GTK.CSS
    ( setCSSName
    , setCSSClass
    , setCSSStyleSheet
    ) where

import GI.Gtk as Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.Useful

setCSSName :: Text -> Widget -> CreateView ()
setCSSName name w = #setName w name

setCSSClass :: Text -> Widget -> CreateView ()
setCSSClass cssclass w = do
    sc <- #getStyleContext w
    #addClass sc cssclass

setCSSStyleSheet :: Bool -> Word32 -> Text -> Widget -> CreateView ()
setCSSStyleSheet full priority css w = do
    provider <- new CssProvider []
    #loadFromData provider $ encodeUtf8 css
    children <- liftIO $ widgetGetTree full w
    for_ children $ \child -> do
        sc <- #getStyleContext child
        #addProvider sc provider priority
