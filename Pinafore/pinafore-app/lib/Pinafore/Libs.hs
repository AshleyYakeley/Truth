module Pinafore.Libs where

import Pinafore.API
import Pinafore.Library.GNOME
import Pinafore.Library.Media
import Pinafore.Main
import Shapes

appLibrary :: [LibraryModule]
appLibrary = pinaforeLibrary <> mediaLibrary <> gnomeLibrary
