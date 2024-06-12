module Pinafore.Library.Media
    ( Media(..)
    , DecodeMedia(..)
    , dataLiteralMediaCodec
    , HTMLText(..)
    , LangDrawing(..)
    , LangImage(..)
    , mediaLibrary
    ) where

import Pinafore.API
import Pinafore.Library.Media.Cairo
import Pinafore.Library.Media.Colour
import Pinafore.Library.Media.CommonMark
import Pinafore.Library.Media.HTML
import Pinafore.Library.Media.Image
import Pinafore.Library.Media.Media
import Shapes

mediaLibrary :: [LibraryModule ()]
mediaLibrary =
    pure $
    MkLibraryModule "media" $
    mconcat [mediaEntityLibSection, htmlStuff, commonMarkStuff, colourStuff, imageStuff, cairoStuff]
