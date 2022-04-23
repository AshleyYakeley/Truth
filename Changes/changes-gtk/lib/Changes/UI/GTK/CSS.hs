module Changes.UI.GTK.CSS
    ( setCSSName
    , setCSSClass
    , bindCSS
    ) where

import Changes.Core
import Changes.GI
import GI.Gtk as GI
import Shapes

setCSSName :: Text -> Widget -> View ()
setCSSName name w = #setName w name

setCSSClass :: Text -> Widget -> View ()
setCSSClass cssclass w = do
    sc <- #getStyleContext w
    #addClass sc cssclass

bindCSS :: Bool -> Word32 -> Model (ROWUpdate Text) -> Widget -> View ()
bindCSS tree priority cssmod widget = do
    provider <- cvNew CssProvider []
    widgets <-
        case tree of
            False -> return [widget]
            True -> liftIO $ widgetGetTree False widget
    for_ widgets $ \w -> do
        sc <- #getStyleContext w
        #addProvider sc provider priority
    viewBindReadOnlyWholeModel cssmod $ \css -> #loadFromData provider $ encodeUtf8 css
