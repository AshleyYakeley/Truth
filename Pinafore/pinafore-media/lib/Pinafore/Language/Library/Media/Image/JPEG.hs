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
    deriving (IsDataLiteral JPEGData)

jpegImageGroundType :: QGroundType '[] LangJPEGImage
jpegImageGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangJPEGImage)|]) "JPEGImage")
        { pgtGreatestDynamicSupertype =
              SimplePolyGreatestDynamicSupertype
                  qGroundType
                  (functionToShim "fromLiteral" jpegFromLiteral)
                  (functionToShim "jpegLiteral" idlLiteral)
        }

instance HasQGroundType '[] LangJPEGImage where
    qGroundType = jpegImageGroundType

jpegDataFromLiteral :: Literal -> Maybe JPEGData
jpegDataFromLiteral (MkMIMELiteral (MkMIMEContentType "image" "jpeg" _) bs) = do
    resultToMaybe $ decode (jpegFormat 0) $ fromStrict bs
jpegDataFromLiteral _ = Nothing

jpegFromLiteral :: Literal -> Maybe LangJPEGImage
jpegFromLiteral lit = do
    idata <- jpegDataFromLiteral lit
    return $ mkDataLiteral lit idata

jpegEncode :: Integer -> [(Text, Literal)] -> LangImage -> LangJPEGImage
jpegEncode q mdata (MkLangImage image) = let
    bs = encode (jpegFormat $ fromInteger q) (metadataToKeyMap mdata, someConvertImage image)
    lit = MkMIMELiteral (MkMIMEContentType "image" "jpeg" []) $ toStrict bs
    idata = fromJust $ jpegDataFromLiteral lit
    in mkDataLiteral lit idata

jpegMetadata :: LangJPEGImage -> LangHasMetadata
jpegMetadata image = keyMapToMetadata $ fst $ idlData image

jpegStuff :: BindDocTree ()
jpegStuff =
    headingBDT
        "JPEG"
        ""
        [ typeBDT "JPEGImage" "An image in JPEG format." (MkSomeGroundType jpegImageGroundType) []
        , hasSubtypeRelationBDT @LangJPEGImage @LangImage Verify "" $
          functionToShim "jpegImage" $ MkLangImage . mapSome toPixelType . snd . idlData
        , hasSubtypeRelationBDT @LangJPEGImage @Literal Verify "" $ functionToShim "jpegLiteral" idlLiteral
        , hasSubtypeRelationBDT @LangJPEGImage @LangHasMetadata Verify "" $ functionToShim "jpegMetadata" jpegMetadata
        , valBDT "jpegEncode" "Encode an image as JPEG, with given quality and metadata." jpegEncode
        ]
