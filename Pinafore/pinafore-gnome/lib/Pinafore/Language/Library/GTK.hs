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

gtkStuff :: DocTreeEntry BindDoc
gtkStuff =
    docTreeEntry "GTK" "User interface, using GTK." $
    namespaceRelative "GTK" [elementStuff, drawingStuff, menuItemStuff, windowStuff, clipboardStuff, dialogStuff]

allGTKStuff :: [DocTreeEntry BindDoc]
allGTKStuff = [gtkStuff, gtkDebugStuff]
