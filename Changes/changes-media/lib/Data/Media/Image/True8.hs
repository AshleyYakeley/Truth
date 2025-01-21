module Data.Media.Image.True8
    ( True8PixelType (..)
    )
where

import Codec.Picture.Types
import Shapes

import Data.Media.Image.Pixel

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
    pixelConvertImage Y8PixelType image = MkSomeFor NoAlphaTrue8PixelType $ promoteImage image
    pixelConvertImage Y16PixelType image = MkSomeFor NoAlphaTrue8PixelType $ promoteImage $ pixelMap pixel16To8 image
    pixelConvertImage Y32PixelType image = MkSomeFor NoAlphaTrue8PixelType $ promoteImage $ pixelMap pixel32To8 image
    pixelConvertImage YFPixelType image = MkSomeFor NoAlphaTrue8PixelType $ promoteImage $ pixelMap pixelFTo8 image
    pixelConvertImage YA8PixelType image = MkSomeFor AlphaTrue8PixelType $ promoteImage image
    pixelConvertImage YA16PixelType image = MkSomeFor AlphaTrue8PixelType $ promoteImage $ pixelMap pixelYA16to8 image
    pixelConvertImage RGB8PixelType image = MkSomeFor NoAlphaTrue8PixelType image
    pixelConvertImage RGB16PixelType image = MkSomeFor NoAlphaTrue8PixelType $ pixelMap pixelRGB16to8 image
    pixelConvertImage RGBFPixelType image = MkSomeFor NoAlphaTrue8PixelType $ pixelMap pixelRGBFto8 image
    pixelConvertImage RGBA8PixelType image = MkSomeFor AlphaTrue8PixelType image
    pixelConvertImage RGBA16PixelType image = MkSomeFor AlphaTrue8PixelType $ pixelMap pixelRGBA16to8 image
    pixelConvertImage YCbCr8PixelType image = MkSomeFor NoAlphaTrue8PixelType $ convertImage image
    pixelConvertImage CMYK8PixelType image = MkSomeFor NoAlphaTrue8PixelType $ convertImage image
    pixelConvertImage CMYK16PixelType image =
        MkSomeFor NoAlphaTrue8PixelType $ pixelMap pixelRGB16to8 $ convertImage image
