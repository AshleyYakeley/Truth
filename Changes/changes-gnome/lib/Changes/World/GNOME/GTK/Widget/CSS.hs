module Changes.World.GNOME.GTK.Widget.CSS
    ( setCSSName
    , setCSSClass
    , bindCSS
    )
where

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

setCSSName :: Text -> GI.Widget -> GView 'Locked ()
setCSSName name w = #setName w name

setCSSClass :: Text -> GI.Widget -> GView 'Locked ()
setCSSClass cssclass w = do
    sc <- #getStyleContext w
    #addClass sc cssclass

bindCSS :: Word32 -> Model (ROWUpdate Text) -> GI.Widget -> GView 'Unlocked ()
bindCSS priority cssmod widget = do
    provider <-
        gvRunLocked $ do
            provider <- gvNew GI.CssProvider []
            sc <- #getStyleContext widget
            #addProvider sc provider priority
            return provider
    gvBindReadOnlyWholeModel cssmod $ \css -> gvRunLocked $ #loadFromData provider $ encodeUtf8 css
