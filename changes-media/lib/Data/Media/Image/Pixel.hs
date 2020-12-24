module Data.Media.Image.Pixel where

import Codec.Picture.Types
import Shapes

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

fromDynamicImage :: DynamicImage -> AnyF PixelType Image
fromDynamicImage (ImageY8 image) = MkAnyF Y8PixelType image
fromDynamicImage (ImageY16 image) = MkAnyF Y16PixelType image
fromDynamicImage (ImageY32 image) = MkAnyF Y32PixelType image
fromDynamicImage (ImageYF image) = MkAnyF YFPixelType image
fromDynamicImage (ImageYA8 image) = MkAnyF YA8PixelType image
fromDynamicImage (ImageYA16 image) = MkAnyF YA16PixelType image
fromDynamicImage (ImageRGB8 image) = MkAnyF RGB8PixelType image
fromDynamicImage (ImageRGB16 image) = MkAnyF RGB16PixelType image
fromDynamicImage (ImageRGBF image) = MkAnyF RGBFPixelType image
fromDynamicImage (ImageRGBA8 image) = MkAnyF RGBA8PixelType image
fromDynamicImage (ImageRGBA16 image) = MkAnyF RGBA16PixelType image
fromDynamicImage (ImageYCbCr8 image) = MkAnyF YCbCr8PixelType image
fromDynamicImage (ImageCMYK8 image) = MkAnyF CMYK8PixelType image
fromDynamicImage (ImageCMYK16 image) = MkAnyF CMYK16PixelType image

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

class BlackWhite px where
    black :: px
    white :: px

instance BlackWhite PixelRGB8 where
    black = PixelRGB8 0 0 0
    white = PixelRGB8 0xFF 0xFF 0xFF

instance BlackWhite PixelRGBA8 where
    black = PixelRGBA8 0 0 0 0xFF
    white = PixelRGBA8 0xFF 0xFF 0xFF 0xFF
