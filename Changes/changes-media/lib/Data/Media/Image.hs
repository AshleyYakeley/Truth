module Data.Media.Image
    ( module I
    , module Data.Media.Image
    ) where

import Codec.Picture.Types as I (Image(..), Pixel(..), PixelRGB16(..), PixelRGB8(..), PixelRGBA16(..), PixelRGBA8(..))
import Codec.Picture.Types
import Data.Media.Image.JPEG as I
import Data.Media.Image.Metadata as I hiding (Keys)
import Data.Media.Image.PNG as I
import Data.Media.Image.Pixel as I (BlackWhite(..), PixelSubtype(..), PixelType(..), someConvertImage)
import Data.Media.Image.True8 as I
import Shapes

blankImage ::
       forall px. Pixel px
    => (Int, Int)
    -> px
    -> Image px
blankImage (w, h) p = generateImage (\_ _ -> p) w h

imageSize :: Image px -> (Int, Int)
imageSize image = (imageWidth image, imageHeight image)
