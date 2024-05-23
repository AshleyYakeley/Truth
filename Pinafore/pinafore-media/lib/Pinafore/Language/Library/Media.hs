module Pinafore.Language.Library.Media
    ( mediaLibrary
    , LangDrawing(..)
    , LangImage(..)
    ) where

import Pinafore.API
import Pinafore.Language.Library.Media.Cairo
import Pinafore.Language.Library.Media.Colour
import Pinafore.Language.Library.Media.Image
import Shapes

mediaLibrary :: [LibraryModule ()]
mediaLibrary = pure $ MkLibraryModule "pinafore-media" $ mconcat [colourStuff, imageStuff, cairoStuff]
