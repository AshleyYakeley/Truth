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

instance WitnessConstraint JpgEncodable JPEGPixelType where
    witnessConstraint Y8JPEGPixelType = Dict
    witnessConstraint RGB8JPEGPixelType = Dict
    witnessConstraint YCbCr8JPEGPixelType = Dict
    witnessConstraint CMYK8JPEGPixelType = Dict

jpegFormat :: Word8 -> ReasonCodec LazyByteString (WitnessDict ImageDataKey, SomeFor JPEGPixelType Image)
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

jpegImageTrue8 :: Applicative m => Codec' m (SomeFor JPEGPixelType Image) (Image PixelRGB8)
jpegImageTrue8 = let
    decode (MkSomeFor pxt image) = pure $ jpegImageToTrue8 pxt image
    encode image = MkSomeFor RGB8JPEGPixelType image
    in MkCodec {..}
