module Pinafore.Language.Library.Media.Image
    ( imageStuff
    , LangImage(..)
    ) where

import Data.Media.Image
import Pinafore.Language.API
import Pinafore.Language.Library.Media.Colour
import Pinafore.Language.Library.Media.Image.Image
import Pinafore.Language.Library.Media.Image.JPEG
import Pinafore.Language.Library.Media.Image.Metadata
import Pinafore.Language.Library.Media.Image.PNG
import Shapes

langImageSize :: LangImage -> (Int, Int)
langImageSize (MkLangImage (MkSomeFor _ image)) = imageSize image

colourToPixel :: LangColour -> PixelRGB16
colourToPixel (ColorSRGB r g b) = PixelRGB16 r g b

alphaColourToPixel :: LangAlphaColour -> PixelRGBA16
alphaColourToPixel (Alpha (ColorSRGB r g b) a) = PixelRGBA16 r g b a

langBlankImage :: LangAlphaColour -> (Int, Int) -> LangImage
langBlankImage (MkOpaqueAlphaColour col) size =
    MkLangImage $ MkSomeFor RGB16PixelType $ blankImage size $ colourToPixel col
langBlankImage acol size = MkLangImage $ MkSomeFor RGBA16PixelType $ blankImage size $ alphaColourToPixel acol

imageStuff :: BindDocStuff ()
imageStuff =
    headingBDS "Image" "" $
    [ typeBDS "Image" "An image." (MkSomeGroundType imageGroundType) []
    , namespaceBDS
          "Image"
          [ valBDS "size" "The size of an image" langImageSize
          , valBDS "blank" "An image of one colour" langBlankImage
          , metadataStuff
          , pngStuff
          , jpegStuff
          ]
    ]
