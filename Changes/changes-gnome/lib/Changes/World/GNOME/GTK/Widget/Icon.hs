module Changes.World.GNOME.GTK.Widget.Icon
    ( IconName
    , GI.IconSize
    , createIcon
    )
where

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

-- | https://specifications.freedesktop.org/icon-naming-spec/icon-naming-spec-latest.html
type IconName = Text

createIcon :: IconName -> GI.IconSize -> GView 'Unlocked GI.Widget
createIcon icon size =
    gvRunLocked $ do
        image <- GI.imageNewFromIconName $ Just icon
        gvAcquire image
        GI.set image [ #iconSize GI.:=  size ]
        GI.toWidget image
