module Pinafore.Library.Media.Image
    ( imageStuff
    , LangImage (..)
    )
where

import Data.Media.Image
import Pinafore.API
import Shapes

import Pinafore.Library.Media.Colour
import Pinafore.Library.Media.Image.Image
import Pinafore.Library.Media.Image.JPEG
import Pinafore.Library.Media.Image.Metadata
import Pinafore.Library.Media.Image.PNG

langImageSize :: LangImage -> (Natural, Natural)
langImageSize (MkLangImage (MkSomeFor _ image)) = imageSize image

colourToPixel :: LangColour -> PixelRGB16
colourToPixel (ColorSRGB r g b) = PixelRGB16 r g b

alphaColourToPixel :: LangAlphaColour -> PixelRGBA16
alphaColourToPixel (Alpha (ColorSRGB r g b) a) = PixelRGBA16 r g b a

langBlankImage :: LangAlphaColour -> (Natural, Natural) -> LangImage
langBlankImage (MkOpaqueAlphaColour col) size =
    MkLangImage $ MkSomeFor RGB16PixelType $ blankImage size $ colourToPixel col
langBlankImage acol size = MkLangImage $ MkSomeFor RGBA16PixelType $ blankImage size $ alphaColourToPixel acol

imageStuff :: LibraryStuff
imageStuff =
    headingBDS "Image" ""
        $ [ typeBDS "Image" "An image." (MkSomeGroundType imageGroundType) []
          , namespaceBDS
                "Image"
                [ valBDS "size" "The size of an image" langImageSize
                , valBDS "blank" "An image of one colour" langBlankImage
                , metadataStuff
                , pngStuff
                , jpegStuff
                ]
          ]
