{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Library.Media.Image.JPEG
    ( jpegStuff
    ) where

import Changes.World.Media
import Data.Media.Image
import Data.Shim
import Pinafore.API
import Pinafore.Library.Media.Image.Image
import Pinafore.Library.Media.Image.Metadata
import Pinafore.Library.Media.Media
import Shapes

type JPEGData = (WitnessMapOf ImageDataKey, SomeFor Image JPEGPixelType)

-- LangJPEGImage
newtype LangJPEGImage =
    MkLangJPEGImage (DataLiteral JPEGData)
    deriving newtype (Eq)

instance AsLiteral LangJPEGImage where
    literalCodec = coerceCodec . literalCodec @(DataLiteral JPEGData)

jpegImageGroundType :: QGroundType '[] LangJPEGImage
jpegImageGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangJPEGImage)|]) "JPEG.Image."

instance HasQGroundType '[] LangJPEGImage where
    qGroundType = jpegImageGroundType

instance DecodeLiteral JPEGData where
    dmLiteralType = generalLiteralType $ dmMediaType @JPEGData
    dmDecode bs = resultToMaybe $ decode (jpegFormat 0) $ fromStrict bs

instance DecodeMedia JPEGData where
    dmMediaType = MkMediaType "image" "jpeg" []
    dmMatchContentType (MkMediaType "image" "jpeg" _) = True
    dmMatchContentType _ = False

jpegEncode :: Natural -> [(Text, Literal)] -> LangImage -> LangJPEGImage
jpegEncode q mdata (MkLangImage image) = let
    dt = (metadataToKeyMap mdata, someConvertImage image)
    bs = toStrict $ encode (jpegFormat $ fromIntegral q) dt
    in MkLangJPEGImage $ bytesToDataLiteral bs

jpegMetadata :: LangJPEGImage -> LangHasMetadata
jpegMetadata (MkLangJPEGImage dl) = keyMapToMetadata $ fst $ dlData dl

jpegImage :: LangJPEGImage -> LangImage
jpegImage (MkLangJPEGImage dl) = MkLangImage $ mapSome toPixelType $ snd $ dlData dl

jpegStuff :: LibraryStuff
jpegStuff =
    headingBDS
        "JPEG"
        ""
        [ typeBDS "JPEG" "An image in JPEG format." (MkSomeGroundType jpegImageGroundType) []
        , hasSubtypeRelationBDS @LangJPEGImage @(Interpret LangImage) Verify "" $
          functionToShim "jpegImage" $ MkInterpret . jpegImage
        , literalSubtypeRelationEntry @LangJPEGImage
        , hasSubtypeRelationBDS @LangJPEGImage @LangHasMetadata Verify "" $ functionToShim "jpegMetadata" jpegMetadata
        , namespaceBDS
              "JPEG"
              [ valBDS "encode" "Encode an image as JPEG, with given quality and metadata." jpegEncode
              , valBDS "jpegMedia" "" $
                codecToPrism $ coerceCodec @_ @(DataLiteral JPEGData) @LangJPEGImage . dataLiteralMediaCodec
              ]
        ]
