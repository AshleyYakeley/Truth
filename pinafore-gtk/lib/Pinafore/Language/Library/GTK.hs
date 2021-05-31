{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.GTK
    ( gtkLibrary
    , LangDrawing
    ) where

import Pinafore.Language.API
import Pinafore.Language.Library.GTK.Debug
import Pinafore.Language.Library.GTK.Drawing
import Pinafore.Language.Library.GTK.Element
import Pinafore.Language.Library.GTK.MenuItem
import Pinafore.Language.Library.GTK.Window

uiLibraryModule :: LibraryModule
uiLibraryModule = MkDocTree "UI" "User interface, using GTK." [elementStuff, menuItemStuff, windowStuff]

gtkLibrary :: [LibraryModule]
gtkLibrary = [drawingLibraryModule, uiLibraryModule, uiDebugLibraryModule]
