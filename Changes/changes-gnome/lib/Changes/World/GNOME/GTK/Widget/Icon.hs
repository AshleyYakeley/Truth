module Changes.World.GNOME.GTK.Widget.Icon
    ( IconName
    , IconSize
    , createIcon
    )
where

import GI.Gtk
import Shapes

import Changes.World.GNOME.GI

-- | https://specifications.freedesktop.org/icon-naming-spec/icon-naming-spec-latest.html
type IconName = Text

createIcon :: IconName -> IconSize -> GView 'Unlocked Widget
createIcon icon size =
    gvRunLocked $ do
        image <- imageNewFromIconName (Just icon) (fromIntegral $ fromEnum size)
        gvAcquire image
        toWidget image
