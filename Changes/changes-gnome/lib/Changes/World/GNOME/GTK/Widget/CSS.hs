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
setCSSClass cssclass w = GI.widgetAddCssClass w cssclass

bindCSS :: Word32 -> Model (ROWUpdate Text) -> GI.Widget -> GView 'Unlocked ()
bindCSS priority cssmod _widget = do
    provider <-
        gvRunLocked $ do
            provider <- gvNew GI.CssProvider []
            mdisplay <- GI.displayGetDefault
            case mdisplay of
                Just display -> do
                    GI.styleContextAddProviderForDisplay display provider priority
                    gvOnClose
                        $ gsvLiftIO
                        $ GI.styleContextRemoveProviderForDisplay display provider
                    return provider
                Nothing -> fail "No GDK display"
    gvBindReadOnlyWholeModel cssmod $ \css ->
        gvRunLocked
            $ GI.cssProviderLoadFromString provider css
