module Changes.World.GNOME.GTK.Widget.TextStyle where

import Import
import Import.GI qualified as GI

data TextStyle = MkTextStyle
    { tsItalic :: Bool
    }
    deriving stock Eq

plainTextStyle :: TextStyle
plainTextStyle = let
    tsItalic = False
    in MkTextStyle{..}

textCellAttributes :: Text -> TextStyle -> [GI.AttrOp GI.CellRendererText 'GI.AttrSet]
textCellAttributes text MkTextStyle{..} =
    [ #text GI.:= text
    , #style
        GI.:= if tsItalic
            then GI.StyleItalic
            else GI.StyleNormal
    ]
