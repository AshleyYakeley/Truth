module Pinafore.Language.Library.GTK
    ( gtkLibrary
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

gtkLibraryModule :: LibraryModule
gtkLibraryModule =
    MkDocTree "GTK" "User interface, using GTK." $
    namespaceRelative "GTK" [elementStuff, drawingStuff, menuItemStuff, windowStuff, clipboardStuff, dialogStuff]

gtkLibrary :: [LibraryModule]
gtkLibrary = [gtkLibraryModule, gtkDebugLibraryModule]
