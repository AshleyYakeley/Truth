module Pinafore.Language.Library.GTK
    ( gtkLibrary
    ) where

import Pinafore.Language.API
import Pinafore.Language.Library.GIO
import Pinafore.Language.Library.GTK.Debug
import Pinafore.Language.Library.GTK.Element
import Pinafore.Language.Library.GTK.Element.Drawing
import Pinafore.Language.Library.GTK.MenuItem
import Pinafore.Language.Library.GTK.Window

gtkLibraryModule :: LibraryModule
gtkLibraryModule = MkDocTree "GTK" "User interface, using GTK." [elementStuff, drawingStuff, menuItemStuff, windowStuff]

gtkLibrary :: [LibraryModule]
gtkLibrary = [gioLibraryModule, gtkLibraryModule, gtkDebugLibraryModule]
