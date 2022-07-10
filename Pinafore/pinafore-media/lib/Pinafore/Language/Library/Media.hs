module Pinafore.Language.Library.Media
    ( mediaLibrary
    , LangDrawing(..)
    ) where

import Pinafore.Language.API
import Pinafore.Language.Library.Media.Colour
import Pinafore.Language.Library.Media.Drawing
import Pinafore.Language.Library.Media.Image

mediaLibrary :: [LibraryModule]
mediaLibrary = [colourLibraryModule, imageLibraryModule, drawingLibraryModule]
