module Truth.UI.GTK.TextStyle where

import Data.GI.Base.Attributes
import Data.GI.Gtk
import GI.Pango
import Shapes
import Truth.Core

textCellAttributes :: Text -> TextStyle -> [AttrOp CellRendererText 'AttrSet]
textCellAttributes text MkTextStyle {..} =
    [ #text := text
    , #style :=
      if tsItalic
          then StyleItalic
          else StyleNormal
    ]
