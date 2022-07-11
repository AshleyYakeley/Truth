module Data.Media.Image.PNG
    ( PNGPixelType(..)
    , pngFormat
    ) where

import Changes.Core
import Codec.Picture.Png
import Codec.Picture.Types
import Data.Media.Image.Metadata
import Data.Media.Image.Pixel
import Shapes

type PNGPixelType :: Type -> Type
data PNGPixelType px where
    Y8PNGPixelType :: PNGPixelType Pixel8
    Y16PNGPixelType :: PNGPixelType Pixel16
    YA8PNGPixelType :: PNGPixelType PixelYA8
    YA16PNGPixelType :: PNGPixelType PixelYA16
    RGB8PNGPixelType :: PNGPixelType PixelRGB8
    RGB16PNGPixelType :: PNGPixelType PixelRGB16
    RGBA8PNGPixelType :: PNGPixelType PixelRGBA8
    RGBA16PNGPixelType :: PNGPixelType PixelRGBA16

instance TestEquality PNGPixelType where
    testEquality Y8PNGPixelType Y8PNGPixelType = Just Refl
    testEquality Y16PNGPixelType Y16PNGPixelType = Just Refl
    testEquality YA8PNGPixelType YA8PNGPixelType = Just Refl
    testEquality YA16PNGPixelType YA16PNGPixelType = Just Refl
    testEquality RGB8PNGPixelType RGB8PNGPixelType = Just Refl
    testEquality RGB16PNGPixelType RGB16PNGPixelType = Just Refl
    testEquality RGBA8PNGPixelType RGBA8PNGPixelType = Just Refl
    testEquality RGBA16PNGPixelType RGBA16PNGPixelType = Just Refl
    testEquality _ _ = Nothing

instance PixelSubtype PNGPixelType where
    toPixelType Y8PNGPixelType = Y8PixelType
    toPixelType Y16PNGPixelType = Y16PixelType
    toPixelType YA8PNGPixelType = YA8PixelType
    toPixelType YA16PNGPixelType = YA16PixelType
    toPixelType RGB8PNGPixelType = RGB8PixelType
    toPixelType RGB16PNGPixelType = RGB16PixelType
    toPixelType RGBA8PNGPixelType = RGBA8PixelType
    toPixelType RGBA16PNGPixelType = RGBA16PixelType
    fromPixelType Y8PixelType = Just Y8PNGPixelType
    fromPixelType Y16PixelType = Just Y16PNGPixelType
    fromPixelType YA8PixelType = Just YA8PNGPixelType
    fromPixelType YA16PixelType = Just YA16PNGPixelType
    fromPixelType RGB8PixelType = Just RGB8PNGPixelType
    fromPixelType RGB16PixelType = Just RGB16PNGPixelType
    fromPixelType RGBA8PixelType = Just RGBA8PNGPixelType
    fromPixelType RGBA16PixelType = Just RGBA16PNGPixelType
    fromPixelType _ = Nothing
    pixelConvertImage Y8PixelType image = MkSomeFor Y8PNGPixelType image
    pixelConvertImage Y16PixelType image = MkSomeFor Y16PNGPixelType image
    pixelConvertImage Y32PixelType image = MkSomeFor Y16PNGPixelType $ pixelMap pixel32To16 image
    pixelConvertImage YFPixelType image = MkSomeFor Y16PNGPixelType $ pixelMap pixelFTo16 image
    pixelConvertImage YA8PixelType image = MkSomeFor YA8PNGPixelType image
    pixelConvertImage YA16PixelType image = MkSomeFor YA16PNGPixelType image
    pixelConvertImage RGB8PixelType image = MkSomeFor RGB8PNGPixelType image
    pixelConvertImage RGB16PixelType image = MkSomeFor RGB16PNGPixelType image
    pixelConvertImage RGBFPixelType image = MkSomeFor RGB16PNGPixelType $ pixelMap pixelRGBFto16 image
    pixelConvertImage RGBA8PixelType image = MkSomeFor RGBA8PNGPixelType image
    pixelConvertImage RGBA16PixelType image = MkSomeFor RGBA16PNGPixelType image
    pixelConvertImage YCbCr8PixelType image = MkSomeFor RGB8PNGPixelType $ convertImage image
    pixelConvertImage CMYK8PixelType image = MkSomeFor RGB8PNGPixelType $ convertImage image
    pixelConvertImage CMYK16PixelType image = MkSomeFor RGB16PNGPixelType $ convertImage image

instance WitnessConstraint PngSavable PNGPixelType where
    witnessConstraint Y8PNGPixelType = Dict
    witnessConstraint Y16PNGPixelType = Dict
    witnessConstraint YA8PNGPixelType = Dict
    witnessConstraint YA16PNGPixelType = Dict
    witnessConstraint RGB8PNGPixelType = Dict
    witnessConstraint RGB16PNGPixelType = Dict
    witnessConstraint RGBA8PNGPixelType = Dict
    witnessConstraint RGBA16PNGPixelType = Dict

pngFormat :: ReasonCodec LazyByteString (WitnessMapOf ImageDataKey, SomeFor Image PNGPixelType)
pngFormat = let
    decode bs =
        case decodePngWithMetadata $ toStrict bs of
            Left err -> FailureResult $ fromString err
            Right (di, mtd) ->
                case fromDynamicImage di of
                    MkSomeFor pxt image
                        | Just xpxt <- fromPixelType pxt -> SuccessResult (fromMetadatas mtd, MkSomeFor xpxt image)
                    _ -> FailureResult "wrong PNG image format"
    encode (mtd, MkSomeFor pxt image) =
        case witnessConstraint @Type @PngSavable pxt of
            Dict -> encodePngWithMetadata (toMetadatas mtd) image
    in MkCodec {..}
