{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.GTK
    ( gtkLibrary
    , LangDrawing
    ) where

import Pinafore.Language.API
import Pinafore.Language.Library.GTK.Colour
import Pinafore.Language.Library.GTK.Debug
import Pinafore.Language.Library.GTK.Drawing
import Pinafore.Language.Library.GTK.Element
import Pinafore.Language.Library.GTK.Image
import Pinafore.Language.Library.GTK.MenuItem
import Pinafore.Language.Library.GTK.Window

uiLibraryModule :: LibraryModule
uiLibraryModule = MkDocTree "GTK" "User interface, using GTK." [elementStuff, menuItemStuff, windowStuff]

gtkLibrary :: [LibraryModule]
gtkLibrary = [colourLibraryModule, imageLibraryModule, drawingLibraryModule, uiLibraryModule, uiDebugLibraryModule]
