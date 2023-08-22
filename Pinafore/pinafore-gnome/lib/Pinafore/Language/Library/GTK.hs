module Pinafore.Language.Library.GTK
    ( allGTKStuff
    , LangContext(..)
    ) where

import Pinafore.Language.API
import Pinafore.Language.Library.GTK.Clipboard
import Pinafore.Language.Library.GTK.Context
import Pinafore.Language.Library.GTK.Debug
import Pinafore.Language.Library.GTK.MenuEntry
import Pinafore.Language.Library.GTK.Widget
import Pinafore.Language.Library.GTK.Widget.Drawing
import Pinafore.Language.Library.GTK.Window
import Shapes

gtkStuff :: BindDocStuff ()
gtkStuff =
    headingBDS "GTK" "User interface, using GTK." $
    pure $ namespaceBDS "GTK" [widgetStuff, drawingStuff, menuEntryStuff, windowStuff, clipboardStuff, dialogStuff]

allGTKStuff :: [BindDocStuff ()]
allGTKStuff = [gtkStuff, gtkDebugStuff]
