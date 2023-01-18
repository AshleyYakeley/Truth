{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Media.Image.PNG
    ( pngStuff
    ) where

import Changes.World.MIME
import Data.Media.Image
import Data.Shim
import Pinafore.Base
import Pinafore.Language.API
import Pinafore.Language.Library.Media.Image.Image
import Pinafore.Language.Library.Media.Image.Metadata
import Shapes

type PNGData = (WitnessMapOf ImageDataKey, SomeFor Image PNGPixelType)

-- LangPNGImage
newtype LangPNGImage =
    MkLangPNGImage (DataLiteral PNGData)
    deriving (IsDataLiteral)

pngImageGroundType :: QGroundType '[] LangPNGImage
pngImageGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangPNGImage)|]) "PNG.Image.")
        { pgtGreatestDynamicSupertype =
              SimplePolyGreatestDynamicSupertype
                  qGroundType
                  (functionToShim "fromLiteral" literalToDataLiteral)
                  (functionToShim "pngLiteral" idlLiteral)
        }

instance HasQGroundType '[] LangPNGImage where
    qGroundType = pngImageGroundType

instance DecodeMIME PNGData where
    dmMatchContentType :: MIMEContentType -> Bool
    dmMatchContentType (MkMIMEContentType "image" "png" _) = True
    dmMatchContentType _ = False
    dmDecode bs = resultToMaybe $ decode pngFormat $ fromStrict bs
    dmLiteralContentType = MkMIMEContentType "image" "png" []

pngEncodeToBytes :: [(Text, Literal)] -> LangImage -> StrictByteString
pngEncodeToBytes mdata (MkLangImage image) =
    toStrict $ encode pngFormat (metadataToKeyMap mdata, someConvertImage image)

pngEncode :: [(Text, Literal)] -> LangImage -> LangPNGImage
pngEncode mdata image = bytesToDataLiteral $ pngEncodeToBytes mdata image

pngMetadata :: LangPNGImage -> LangHasMetadata
pngMetadata image = keyMapToMetadata $ fst $ idlData image

pngStuff :: BindDocTree ()
pngStuff =
    headingBDT
        "PNG"
        ""
        [ typeBDT "PNG" "An image in PNG format." (MkSomeGroundType pngImageGroundType) []
        , hasSubtypeRelationBDT @LangPNGImage @(Interpret LangImage) Verify "" $
          functionToShim "pngImage" $ MkInterpret . MkLangImage . mapSome toPixelType . snd . idlData
        , hasSubtypeRelationBDT @LangPNGImage @Literal Verify "" $ functionToShim "pngLiteral" idlLiteral
        , hasSubtypeRelationBDT @LangPNGImage @LangHasMetadata Verify "" $ functionToShim "pngMetadata" pngMetadata
        , namespaceBDT
              "PNG"
              ""
              [ valBDT "encode" "Encode an image as PNG, with given metadata." pngEncode
              , valBDT "pngMIME" "" $ dataLiteralMIMEPrism @LangPNGImage
              ]
        ]
