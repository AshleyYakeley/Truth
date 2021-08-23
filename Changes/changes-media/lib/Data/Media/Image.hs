module Data.Media.Image
    ( module I
    , module Data.Media.Image
    ) where

import Codec.Picture.Types as I (Image(..), Pixel(..), PixelRGB8, PixelRGBA8)
import Codec.Picture.Types
import Data.Media.Image.JPEG as I
import Data.Media.Image.Metadata as I
import Data.Media.Image.Pixel as I (BlackWhite(..), PixelType(..))
import Data.Media.Image.True8 as I
import Shapes

blankImage ::
       forall px. Pixel px
    => (Int, Int)
    -> px
    -> Image px
blankImage (w, h) p = generateImage (\_ _ -> p) w h
