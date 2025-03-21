module Changes.World.GNOME.GTK.Widget.TextStyle where

import Data.GI.Base.Attributes
import Data.GI.Gtk
import GI.Pango
import Shapes

data TextStyle = MkTextStyle
    { tsItalic :: Bool
    }
    deriving stock Eq

plainTextStyle :: TextStyle
plainTextStyle = let
    tsItalic = False
    in MkTextStyle{..}

textCellAttributes :: Text -> TextStyle -> [AttrOp CellRendererText 'AttrSet]
textCellAttributes text MkTextStyle{..} =
    [ #text := text
    , #style
        := if tsItalic
            then StyleItalic
            else StyleNormal
    ]
