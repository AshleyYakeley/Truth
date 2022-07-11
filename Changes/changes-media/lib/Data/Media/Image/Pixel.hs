module Data.Media.Image.Pixel where

import Codec.Picture.Types
import Shapes

pixel16To8 :: Pixel16 -> Pixel8
pixel16To8 = hi

pixel32To16 :: Word32 -> Pixel16
pixel32To16 = hi

pixel32To8 :: Pixel32 -> Pixel8
pixel32To8 = pixel16To8 . pixel32To16

pixelFTo8 :: PixelF -> Pixel8
pixelFTo8 f = round $ f * 0xFF

pixelFTo16 :: PixelF -> Pixel16
pixelFTo16 f = round $ f * 0xFFFF

pixelRGBA16to8 :: PixelRGBA16 -> PixelRGBA8
pixelRGBA16to8 (PixelRGBA16 r g b a) = PixelRGBA8 (pixel16To8 r) (pixel16To8 g) (pixel16To8 b) (pixel16To8 a)

pixelRGB16to8 :: PixelRGB16 -> PixelRGB8
pixelRGB16to8 (PixelRGB16 r g b) = PixelRGB8 (pixel16To8 r) (pixel16To8 g) (pixel16To8 b)

pixelRGBFto16 :: PixelRGBF -> PixelRGB16
pixelRGBFto16 (PixelRGBF r g b) = PixelRGB16 (pixelFTo16 r) (pixelFTo16 g) (pixelFTo16 b)

pixelRGBFto8 :: PixelRGBF -> PixelRGB8
pixelRGBFto8 (PixelRGBF r g b) = PixelRGB8 (pixelFTo8 r) (pixelFTo8 g) (pixelFTo8 b)

pixelYA16to8 :: PixelYA16 -> PixelYA8
pixelYA16to8 (PixelYA16 y a) = PixelYA8 (pixel16To8 y) (pixel16To8 a)

pixelCMYK16to8 :: PixelCMYK16 -> PixelCMYK8
pixelCMYK16to8 (PixelCMYK16 c m y k) = PixelCMYK8 (pixel16To8 c) (pixel16To8 m) (pixel16To8 y) (pixel16To8 k)

type PixelType :: Type -> Type
data PixelType px where
    Y8PixelType :: PixelType Pixel8
    Y16PixelType :: PixelType Pixel16
    Y32PixelType :: PixelType Pixel32
    YFPixelType :: PixelType PixelF
    YA8PixelType :: PixelType PixelYA8
    YA16PixelType :: PixelType PixelYA16
    RGB8PixelType :: PixelType PixelRGB8
    RGB16PixelType :: PixelType PixelRGB16
    RGBFPixelType :: PixelType PixelRGBF
    RGBA8PixelType :: PixelType PixelRGBA8
    RGBA16PixelType :: PixelType PixelRGBA16
    YCbCr8PixelType :: PixelType PixelYCbCr8
    CMYK8PixelType :: PixelType PixelCMYK8
    CMYK16PixelType :: PixelType PixelCMYK16

instance TestEquality PixelType where
    testEquality Y8PixelType Y8PixelType = Just Refl
    testEquality Y16PixelType Y16PixelType = Just Refl
    testEquality Y32PixelType Y32PixelType = Just Refl
    testEquality YFPixelType YFPixelType = Just Refl
    testEquality YA8PixelType YA8PixelType = Just Refl
    testEquality YA16PixelType YA16PixelType = Just Refl
    testEquality RGB8PixelType RGB8PixelType = Just Refl
    testEquality RGB16PixelType RGB16PixelType = Just Refl
    testEquality RGBFPixelType RGBFPixelType = Just Refl
    testEquality RGBA8PixelType RGBA8PixelType = Just Refl
    testEquality RGBA16PixelType RGBA16PixelType = Just Refl
    testEquality YCbCr8PixelType YCbCr8PixelType = Just Refl
    testEquality CMYK8PixelType CMYK8PixelType = Just Refl
    testEquality CMYK16PixelType CMYK16PixelType = Just Refl
    testEquality _ _ = Nothing

fromDynamicImage :: DynamicImage -> SomeFor Image PixelType
fromDynamicImage (ImageY8 image) = MkSomeFor Y8PixelType image
fromDynamicImage (ImageY16 image) = MkSomeFor Y16PixelType image
fromDynamicImage (ImageY32 image) = MkSomeFor Y32PixelType image
fromDynamicImage (ImageYF image) = MkSomeFor YFPixelType image
fromDynamicImage (ImageYA8 image) = MkSomeFor YA8PixelType image
fromDynamicImage (ImageYA16 image) = MkSomeFor YA16PixelType image
fromDynamicImage (ImageRGB8 image) = MkSomeFor RGB8PixelType image
fromDynamicImage (ImageRGB16 image) = MkSomeFor RGB16PixelType image
fromDynamicImage (ImageRGBF image) = MkSomeFor RGBFPixelType image
fromDynamicImage (ImageRGBA8 image) = MkSomeFor RGBA8PixelType image
fromDynamicImage (ImageRGBA16 image) = MkSomeFor RGBA16PixelType image
fromDynamicImage (ImageYCbCr8 image) = MkSomeFor YCbCr8PixelType image
fromDynamicImage (ImageCMYK8 image) = MkSomeFor CMYK8PixelType image
fromDynamicImage (ImageCMYK16 image) = MkSomeFor CMYK16PixelType image

toDynamicImage :: PixelType px -> Image px -> DynamicImage
toDynamicImage Y8PixelType image = ImageY8 image
toDynamicImage Y16PixelType image = ImageY16 image
toDynamicImage Y32PixelType image = ImageY32 image
toDynamicImage YFPixelType image = ImageYF image
toDynamicImage YA8PixelType image = ImageYA8 image
toDynamicImage YA16PixelType image = ImageYA16 image
toDynamicImage RGB8PixelType image = ImageRGB8 image
toDynamicImage RGB16PixelType image = ImageRGB16 image
toDynamicImage RGBFPixelType image = ImageRGBF image
toDynamicImage RGBA8PixelType image = ImageRGBA8 image
toDynamicImage RGBA16PixelType image = ImageRGBA16 image
toDynamicImage YCbCr8PixelType image = ImageYCbCr8 image
toDynamicImage CMYK8PixelType image = ImageCMYK8 image
toDynamicImage CMYK16PixelType image = ImageCMYK16 image

type PixelSubtype :: (Type -> Type) -> Constraint
class PixelSubtype t where
    toPixelType :: forall px. t px -> PixelType px
    fromPixelType :: forall px. PixelType px -> Maybe (t px)
    pixelConvertImage :: forall px. PixelType px -> Image px -> SomeFor Image t

someConvertImage :: PixelSubtype t => SomeFor Image PixelType -> SomeFor Image t
someConvertImage (MkSomeFor pt image) = pixelConvertImage pt image

class BlackWhite px where
    black :: px
    white :: px

instance BlackWhite PixelRGB8 where
    black = PixelRGB8 0 0 0
    white = PixelRGB8 0xFF 0xFF 0xFF

instance BlackWhite PixelRGBA8 where
    black = PixelRGBA8 0 0 0 0xFF
    white = PixelRGBA8 0xFF 0xFF 0xFF 0xFF
