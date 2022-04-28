module Changes.UI.GTK.Icon
    ( IconName
    , IconSize
    , createIcon
    ) where

import Changes.Core
import Changes.GI
import GI.Gtk
import Shapes

-- | https://specifications.freedesktop.org/icon-naming-spec/icon-naming-spec-latest.html
type IconName = Text

createIcon :: IconName -> IconSize -> View Widget
createIcon icon size = do
    image <- imageNewFromIconName (Just icon) (fromIntegral $ fromEnum size)
    cvAcquire image
    toWidget image
