module Pinafore.Language.Library.GTK
    ( allGTKStuff
    , LangContext(..)
    ) where

import Pinafore.Language.API
import Pinafore.Language.Library.GTK.Clipboard
import Pinafore.Language.Library.GTK.Context
import Pinafore.Language.Library.GTK.Debug
import Pinafore.Language.Library.GTK.Element
import Pinafore.Language.Library.GTK.Element.Drawing
import Pinafore.Language.Library.GTK.MenuItem
import Pinafore.Language.Library.GTK.Window
import Shapes

gtkStuff :: BindDocStuff ()
gtkStuff =
    headingBDS "GTK" "User interface, using GTK." $
    pure $ namespaceBDS "GTK" [elementStuff, drawingStuff, menuItemStuff, windowStuff, clipboardStuff, dialogStuff]

allGTKStuff :: [BindDocStuff ()]
allGTKStuff = [gtkStuff, gtkDebugStuff]
