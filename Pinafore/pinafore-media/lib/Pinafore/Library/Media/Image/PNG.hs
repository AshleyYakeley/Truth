{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Library.Media.Image.PNG
    ( pngStuff
    ) where

import Changes.World.Media
import Data.Media.Image
import Data.Shim
import Pinafore.API
import Pinafore.Library.Media.Image.Image
import Pinafore.Library.Media.Image.Metadata
import Shapes

type PNGData = (WitnessMapOf ImageDataKey, SomeFor Image PNGPixelType)

-- LangPNGImage
newtype LangPNGImage =
    MkLangPNGImage (DataLiteral PNGData)
    deriving newtype (Eq)

instance AsLiteral LangPNGImage where
    literalCodec = coerceCodec . literalCodec @(DataLiteral PNGData)

pngImageGroundType :: QGroundType '[] LangPNGImage
pngImageGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangPNGImage)|]) "PNG.Image."

instance HasQGroundType '[] LangPNGImage where
    qGroundType = pngImageGroundType

instance DecodeMedia PNGData where
    dmContentType = MkMediaType "image" "png" []
    dmMatchContentType :: MediaType -> Bool
    dmMatchContentType (MkMediaType "image" "png" _) = True
    dmMatchContentType _ = False
    dmDecode bs = resultToMaybe $ decode pngFormat $ fromStrict bs

pngEncode :: [(Text, Literal)] -> LangImage -> LangPNGImage
pngEncode mdata (MkLangImage image) = let
    dt = (metadataToKeyMap mdata, someConvertImage image)
    bs = toStrict $ encode pngFormat dt
    in MkLangPNGImage $ bytesToDataLiteral bs

pngMetadata :: LangPNGImage -> LangHasMetadata
pngMetadata (MkLangPNGImage dl) = keyMapToMetadata $ fst $ dlData dl

pngImage :: LangPNGImage -> LangImage
pngImage (MkLangPNGImage dl) = MkLangImage $ mapSome toPixelType $ snd $ dlData dl

pngStuff :: LibraryStuff ()
pngStuff =
    headingBDS
        "PNG"
        ""
        [ typeBDS "PNG" "An image in PNG format." (MkSomeGroundType pngImageGroundType) []
        , hasSubtypeRelationBDS @LangPNGImage @(Interpret LangImage) Verify "" $
          functionToShim "pngImage" $ MkInterpret . pngImage
        , literalSubtypeRelationEntry @LangPNGImage
        , hasSubtypeRelationBDS @LangPNGImage @LangHasMetadata Verify "" $ functionToShim "pngMetadata" pngMetadata
        , namespaceBDS
              "PNG"
              [ valBDS "encode" "Encode an image as PNG, with given metadata." pngEncode
              , valBDS "pngMedia" "" $
                codecToPrism $ coerceCodec @_ @(DataLiteral PNGData) @LangPNGImage . dataLiteralMediaCodec
              ]
        ]
