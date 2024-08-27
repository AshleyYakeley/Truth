module Pinafore.Library.GTK
    ( allGTKStuff
    , LangContext(..)
    ) where

import Pinafore.API
import Pinafore.Library.GTK.Clipboard
import Pinafore.Library.GTK.Context
import Pinafore.Library.GTK.Debug
import Pinafore.Library.GTK.MenuEntry
import Pinafore.Library.GTK.Widget
import Pinafore.Library.GTK.Widget.Drawing
import Pinafore.Library.GTK.Window
import Shapes

gtkStuff :: LibraryStuff
gtkStuff =
    headingBDS "GTK" "User interface, using GTK." $
    pure $ namespaceBDS "GTK" [widgetStuff, drawingStuff, menuEntryStuff, windowStuff, clipboardStuff, dialogStuff]

allGTKStuff :: [LibraryStuff]
allGTKStuff = [gtkStuff, gtkDebugStuff]
