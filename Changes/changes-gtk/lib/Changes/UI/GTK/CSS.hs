module Changes.UI.GTK.CSS
    ( setCSSName
    , setCSSClass
    , bindCSS
    ) where

import Changes.Core
import Changes.GI
import GI.Gtk as GI
import Shapes

setCSSName :: Text -> Widget -> CreateView ()
setCSSName name w = #setName w name

setCSSClass :: Text -> Widget -> CreateView ()
setCSSClass cssclass w = do
    sc <- #getStyleContext w
    #addClass sc cssclass

bindCSS :: Bool -> Word32 -> Model (ROWUpdate Text) -> Widget -> CreateView ()
bindCSS tree priority cssmod widget = do
    provider <- cvNew CssProvider []
    widgets <-
        case tree of
            False -> return [widget]
            True -> liftIO $ widgetGetTree False widget
    for_ widgets $ \w -> do
        sc <- #getStyleContext w
        #addProvider sc provider priority
    cvBindReadOnlyWholeModel cssmod $ \css -> #loadFromData provider $ encodeUtf8 css
