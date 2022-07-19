module Changes.World.GNOME.GTK.Element.Icon
    ( IconName
    , IconSize
    , createIcon
    ) where

import Changes.World.GNOME.GI
import GI.Gtk
import Shapes

-- | https://specifications.freedesktop.org/icon-naming-spec/icon-naming-spec-latest.html
type IconName = Text

createIcon :: IconName -> IconSize -> GView 'Unlocked Widget
createIcon icon size =
    gvRunLocked $ do
        image <- imageNewFromIconName (Just icon) (fromIntegral $ fromEnum size)
        gvAcquire image
        toWidget image
