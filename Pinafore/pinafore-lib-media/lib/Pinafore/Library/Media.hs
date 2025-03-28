module Pinafore.Library.Media
    ( Media (..)
    , DecodeMedia (..)
    , dataLiteralMediaCodec
    , HTMLText (..)
    , CSSText (..)
    , LangDrawing (..)
    , LangImage (..)
    , mediaLibrary
    )
where

import Pinafore.API
import Shapes

import Pinafore.Library.Media.CSS
import Pinafore.Library.Media.Cairo
import Pinafore.Library.Media.Colour
import Pinafore.Library.Media.CommonMark
import Pinafore.Library.Media.HTML
import Pinafore.Library.Media.Image
import Pinafore.Library.Media.Media
import Pinafore.Library.Media.URI

mediaLibrary :: [LibraryModule]
mediaLibrary =
    pure
        $ MkLibraryModule "media"
        $ mconcat [mediaEntityLibSection, uriStuff, htmlStuff, cssStuff, commonMarkStuff, colourStuff, imageStuff, cairoStuff]
