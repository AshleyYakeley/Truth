module Truth.UI.GTK.Icon
    ( IconName
    , IconSize
    , createIcon
    ) where

import GI.Gtk
import Shapes
import Truth.Core

-- | https://specifications.freedesktop.org/icon-naming-spec/icon-naming-spec-latest.html
type IconName = Text

createIcon :: IconName -> IconSize -> CreateView Widget
createIcon icon size = do
    image <- imageNewFromIconName (Just icon) (fromIntegral $ fromEnum size)
    toWidget image
