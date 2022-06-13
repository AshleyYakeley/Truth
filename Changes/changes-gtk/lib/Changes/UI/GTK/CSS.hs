module Changes.UI.GTK.CSS
    ( setCSSName
    , setCSSClass
    , bindCSS
    ) where

import Changes.Core
import Changes.GI
import GI.Gtk as GI
import Shapes

setCSSName :: Text -> Widget -> GView 'Locked ()
setCSSName name w = #setName w name

setCSSClass :: Text -> Widget -> GView 'Locked ()
setCSSClass cssclass w = do
    sc <- #getStyleContext w
    #addClass sc cssclass

bindCSS :: Bool -> Word32 -> Model (ROWUpdate Text) -> Widget -> GView 'Locked ()
bindCSS tree priority cssmod widget = do
    provider <- gvNew CssProvider []
    widgets <-
        case tree of
            False -> return [widget]
            True -> liftIO $ widgetGetTree False widget
    for_ widgets $ \w -> do
        sc <- #getStyleContext w
        #addProvider sc provider priority
    gvBindReadOnlyWholeModel cssmod $ \css -> gvRunLocked $ #loadFromData provider $ encodeUtf8 css
