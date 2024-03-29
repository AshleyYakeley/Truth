{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Media.Image.JPEG
    ( jpegStuff
    ) where

import Changes.World.MIME
import Data.Media.Image
import Data.Shim
import Pinafore.Base
import Pinafore.Language.API
import Pinafore.Language.Library.Media.Image.Image
import Pinafore.Language.Library.Media.Image.Metadata
import Shapes

type JPEGData = (WitnessMapOf ImageDataKey, SomeFor Image JPEGPixelType)

-- LangJPEGImage
newtype LangJPEGImage =
    MkLangJPEGImage (DataLiteral JPEGData)
    deriving (IsDataLiteral)

jpegImageGroundType :: QGroundType '[] LangJPEGImage
jpegImageGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangJPEGImage)|]) "JPEG.Image.")
        { qgtGreatestDynamicSupertype =
              SimplePolyGreatestDynamicSupertype
                  qGroundType
                  (functionToShim "fromLiteral" literalToDataLiteral)
                  (functionToShim "jpegLiteral" idlLiteral)
        }

instance HasQGroundType '[] LangJPEGImage where
    qGroundType = jpegImageGroundType

instance DecodeMIME JPEGData where
    dmMatchContentType :: MIMEContentType -> Bool
    dmMatchContentType (MkMIMEContentType "image" "jpeg" _) = True
    dmMatchContentType _ = False
    dmDecode bs = resultToMaybe $ decode (jpegFormat 0) $ fromStrict bs
    dmLiteralContentType = MkMIMEContentType "image" "jpeg" []

jpegEncodeToBytes :: Integer -> [(Text, Literal)] -> LangImage -> StrictByteString
jpegEncodeToBytes q mdata (MkLangImage image) =
    toStrict $ encode (jpegFormat $ fromInteger q) (metadataToKeyMap mdata, someConvertImage image)

jpegEncode :: Integer -> [(Text, Literal)] -> LangImage -> LangJPEGImage
jpegEncode q mdata image = bytesToDataLiteral $ jpegEncodeToBytes q mdata image

jpegMetadata :: LangJPEGImage -> LangHasMetadata
jpegMetadata image = keyMapToMetadata $ fst $ idlData image

jpegStuff :: BindDocStuff ()
jpegStuff =
    headingBDS
        "JPEG"
        ""
        [ typeBDS "JPEG" "An image in JPEG format." (MkSomeGroundType jpegImageGroundType) []
        , hasSubtypeRelationBDS @LangJPEGImage @(Interpret LangImage) Verify "" $
          functionToShim "jpegImage" $ MkInterpret . MkLangImage . mapSome toPixelType . snd . idlData
        , hasSubtypeRelationBDS @LangJPEGImage @Literal Verify "" $ functionToShim "jpegLiteral" idlLiteral
        , hasSubtypeRelationBDS @LangJPEGImage @LangHasMetadata Verify "" $ functionToShim "jpegMetadata" jpegMetadata
        , namespaceBDS
              "JPEG"
              [ valBDS "encode" "Encode an image as JPEG, with given quality and metadata." jpegEncode
              , valBDS "jpegMIME" "" $ dataLiteralMIMEPrism @LangJPEGImage
              ]
        ]
