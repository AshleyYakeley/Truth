module Data.Media.Image.JPEG
    ( JPEGPixelType(..)
    , jpegFormat
    , jpegImageTrue8
    ) where

import Changes.Core
import Codec.Picture.Jpg
import Codec.Picture.Types
import Data.Media.Image.Metadata
import Data.Media.Image.Pixel
import Shapes

type JPEGPixelType :: Type -> Type
data JPEGPixelType px where
    Y8JPEGPixelType :: JPEGPixelType Pixel8
    RGB8JPEGPixelType :: JPEGPixelType PixelRGB8
    YCbCr8JPEGPixelType :: JPEGPixelType PixelYCbCr8
    CMYK8JPEGPixelType :: JPEGPixelType PixelCMYK8

instance TestEquality JPEGPixelType where
    testEquality Y8JPEGPixelType Y8JPEGPixelType = Just Refl
    testEquality RGB8JPEGPixelType RGB8JPEGPixelType = Just Refl
    testEquality YCbCr8JPEGPixelType YCbCr8JPEGPixelType = Just Refl
    testEquality CMYK8JPEGPixelType CMYK8JPEGPixelType = Just Refl
    testEquality _ _ = Nothing

instance PixelSubtype JPEGPixelType where
    toPixelType Y8JPEGPixelType = Y8PixelType
    toPixelType RGB8JPEGPixelType = RGB8PixelType
    toPixelType YCbCr8JPEGPixelType = YCbCr8PixelType
    toPixelType CMYK8JPEGPixelType = CMYK8PixelType
    fromPixelType Y8PixelType = Just Y8JPEGPixelType
    fromPixelType RGB8PixelType = Just RGB8JPEGPixelType
    fromPixelType YCbCr8PixelType = Just YCbCr8JPEGPixelType
    fromPixelType CMYK8PixelType = Just CMYK8JPEGPixelType
    fromPixelType _ = Nothing
    pixelConvertImage Y8PixelType image = MkSomeFor Y8JPEGPixelType image
    pixelConvertImage Y16PixelType image = MkSomeFor Y8JPEGPixelType $ pixelMap pixel16To8 image
    pixelConvertImage Y32PixelType image = MkSomeFor Y8JPEGPixelType $ pixelMap pixel32To8 image
    pixelConvertImage YFPixelType image = MkSomeFor Y8JPEGPixelType $ pixelMap pixelFTo8 image
    pixelConvertImage YA8PixelType image = MkSomeFor Y8JPEGPixelType $ dropAlphaLayer image
    pixelConvertImage YA16PixelType image = MkSomeFor Y8JPEGPixelType $ pixelMap pixel16To8 $ dropAlphaLayer image
    pixelConvertImage RGB8PixelType image = MkSomeFor RGB8JPEGPixelType image
    pixelConvertImage RGB16PixelType image = MkSomeFor RGB8JPEGPixelType $ pixelMap pixelRGB16to8 image
    pixelConvertImage RGBFPixelType image = MkSomeFor RGB8JPEGPixelType $ pixelMap pixelRGBFto8 image
    pixelConvertImage RGBA8PixelType image = MkSomeFor RGB8JPEGPixelType $ dropAlphaLayer image
    pixelConvertImage RGBA16PixelType image =
        MkSomeFor RGB8JPEGPixelType $ pixelMap pixelRGB16to8 $ dropAlphaLayer image
    pixelConvertImage YCbCr8PixelType image = MkSomeFor YCbCr8JPEGPixelType image
    pixelConvertImage CMYK8PixelType image = MkSomeFor CMYK8JPEGPixelType image
    pixelConvertImage CMYK16PixelType image = MkSomeFor CMYK8JPEGPixelType $ pixelMap pixelCMYK16to8 image

instance WitnessConstraint JpgEncodable JPEGPixelType where
    witnessConstraint Y8JPEGPixelType = Dict
    witnessConstraint RGB8JPEGPixelType = Dict
    witnessConstraint YCbCr8JPEGPixelType = Dict
    witnessConstraint CMYK8JPEGPixelType = Dict

jpegFormat :: Word8 -> ReasonCodec LazyByteString (WitnessMapOf ImageDataKey, SomeFor Image JPEGPixelType)
jpegFormat quality = let
    decode bs =
        case decodeJpegWithMetadata $ toStrict bs of
            Left err -> FailureResult $ fromString err
            Right (di, mtd) ->
                case fromDynamicImage di of
                    MkSomeFor pxt image
                        | Just jpxt <- fromPixelType pxt -> SuccessResult (fromMetadatas mtd, MkSomeFor jpxt image)
                    _ -> FailureResult "wrong JPEG image format"
    encode (mtd, MkSomeFor pxt image) =
        case witnessConstraint @Type @JpgEncodable pxt of
            Dict -> encodeDirectJpegAtQualityWithMetadata quality (toMetadatas mtd) image
    in MkCodec {..}

jpegImageToTrue8 :: JPEGPixelType px -> Image px -> Image PixelRGB8
jpegImageToTrue8 Y8JPEGPixelType image = promoteImage image
jpegImageToTrue8 RGB8JPEGPixelType image = image
jpegImageToTrue8 YCbCr8JPEGPixelType image = convertImage image
jpegImageToTrue8 CMYK8JPEGPixelType image = convertImage image

jpegImageTrue8 :: Applicative m => Codec' m (SomeFor Image JPEGPixelType) (Image PixelRGB8)
jpegImageTrue8 = let
    decode (MkSomeFor pxt image) = pure $ jpegImageToTrue8 pxt image
    encode image = MkSomeFor RGB8JPEGPixelType image
    in MkCodec {..}
