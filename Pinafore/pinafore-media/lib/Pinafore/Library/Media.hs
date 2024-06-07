module Pinafore.Library.Media
    ( mediaLibrary
    , HTMLText(..)
    , LangDrawing(..)
    , LangImage(..)
    ) where

import Pinafore.API
import Pinafore.Library.Media.Cairo
import Pinafore.Library.Media.Colour
import Pinafore.Library.Media.HTML
import Pinafore.Library.Media.Image
import Shapes

mediaLibrary :: [LibraryModule ()]
mediaLibrary = pure $ MkLibraryModule "pinafore-media" $ mconcat [htmlStuff, colourStuff, imageStuff, cairoStuff]
