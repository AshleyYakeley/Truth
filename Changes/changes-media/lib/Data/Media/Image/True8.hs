module Data.Media.Image.True8
    ( True8PixelType(..)
    , imageToTrue8
    ) where

import Codec.Picture.Types
import Data.Media.Image.Pixel
import Shapes

type True8PixelType :: Type -> Type
data True8PixelType px where
    NoAlphaTrue8PixelType :: True8PixelType PixelRGB8
    AlphaTrue8PixelType :: True8PixelType PixelRGBA8

instance TestEquality True8PixelType where
    testEquality NoAlphaTrue8PixelType NoAlphaTrue8PixelType = Just Refl
    testEquality AlphaTrue8PixelType AlphaTrue8PixelType = Just Refl
    testEquality _ _ = Nothing

instance PixelSubtype True8PixelType where
    toPixelType NoAlphaTrue8PixelType = RGB8PixelType
    toPixelType AlphaTrue8PixelType = RGBA8PixelType
    fromPixelType RGB8PixelType = Just NoAlphaTrue8PixelType
    fromPixelType RGBA8PixelType = Just AlphaTrue8PixelType
    fromPixelType _ = Nothing

pixel16To8 :: Pixel16 -> Pixel8
pixel16To8 = hi

pixel32To8 :: Pixel32 -> Pixel8
pixel32To8 = hi . hi

pixelFTo8 :: PixelF -> Pixel8
pixelFTo8 f = round $ f * 255

pixelRGBA16to8 :: PixelRGBA16 -> PixelRGBA8
pixelRGBA16to8 (PixelRGBA16 r g b a) = PixelRGBA8 (pixel16To8 r) (pixel16To8 g) (pixel16To8 b) (pixel16To8 a)

pixelRGB16to8 :: PixelRGB16 -> PixelRGB8
pixelRGB16to8 (PixelRGB16 r g b) = PixelRGB8 (pixel16To8 r) (pixel16To8 g) (pixel16To8 b)

pixelRGBFto8 :: PixelRGBF -> PixelRGB8
pixelRGBFto8 (PixelRGBF r g b) = PixelRGB8 (pixelFTo8 r) (pixelFTo8 g) (pixelFTo8 b)

pixelYA16to8 :: PixelYA16 -> PixelYA8
pixelYA16to8 (PixelYA16 y a) = PixelYA8 (pixel16To8 y) (pixel16To8 a)

pixelCMYK16to8 :: PixelCMYK16 -> PixelCMYK8
pixelCMYK16to8 (PixelCMYK16 c m y k) = PixelCMYK8 (pixel16To8 c) (pixel16To8 m) (pixel16To8 y) (pixel16To8 k)

imageToTrue8 :: PixelType px -> Image px -> AnyF True8PixelType Image
imageToTrue8 Y8PixelType image = MkAnyF NoAlphaTrue8PixelType $ promoteImage image
imageToTrue8 Y16PixelType image = MkAnyF NoAlphaTrue8PixelType $ promoteImage $ pixelMap pixel16To8 image
imageToTrue8 Y32PixelType image = MkAnyF NoAlphaTrue8PixelType $ promoteImage $ pixelMap pixel32To8 image
imageToTrue8 YFPixelType image = MkAnyF NoAlphaTrue8PixelType $ promoteImage $ pixelMap pixelFTo8 image
imageToTrue8 YA8PixelType image = MkAnyF AlphaTrue8PixelType $ promoteImage image
imageToTrue8 YA16PixelType image = MkAnyF AlphaTrue8PixelType $ promoteImage $ pixelMap pixelYA16to8 image
imageToTrue8 RGB8PixelType image = MkAnyF NoAlphaTrue8PixelType image
imageToTrue8 RGB16PixelType image = MkAnyF NoAlphaTrue8PixelType $ pixelMap pixelRGB16to8 image
imageToTrue8 RGBFPixelType image = MkAnyF NoAlphaTrue8PixelType $ pixelMap pixelRGBFto8 image
imageToTrue8 RGBA8PixelType image = MkAnyF AlphaTrue8PixelType image
imageToTrue8 RGBA16PixelType image = MkAnyF AlphaTrue8PixelType $ pixelMap pixelRGBA16to8 image
imageToTrue8 YCbCr8PixelType image = MkAnyF NoAlphaTrue8PixelType $ convertImage image
imageToTrue8 CMYK8PixelType image = MkAnyF NoAlphaTrue8PixelType $ convertImage image
imageToTrue8 CMYK16PixelType image = MkAnyF NoAlphaTrue8PixelType $ convertImage $ pixelMap pixelCMYK16to8 image
