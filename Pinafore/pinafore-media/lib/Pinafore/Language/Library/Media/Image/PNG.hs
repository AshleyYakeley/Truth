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
    deriving (IsDataLiteral PNGData)

pngImageGroundType :: QGroundType '[] LangPNGImage
pngImageGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangPNGImage)|]) "PNGImage")
        { pgtGreatestDynamicSupertype =
              SimplePolyGreatestDynamicSupertype
                  qGroundType
                  (functionToShim "fromLiteral" pngFromLiteral)
                  (functionToShim "pngLiteral" idlLiteral)
        }

instance HasQGroundType '[] LangPNGImage where
    qGroundType = pngImageGroundType

pngDataFromLiteral :: Literal -> Maybe PNGData
pngDataFromLiteral (MkMIMELiteral (MkMIMEContentType "image" "png" _) bs) = do
    resultToMaybe $ decode pngFormat $ fromStrict bs
pngDataFromLiteral _ = Nothing

pngFromLiteral :: Literal -> Maybe LangPNGImage
pngFromLiteral lit = do
    idata <- pngDataFromLiteral lit
    return $ mkDataLiteral lit idata

pngEncode :: [(Text, Literal)] -> LangImage -> LangPNGImage
pngEncode mdata (MkLangImage image) = let
    bs = encode pngFormat (metadataToKeyMap mdata, someConvertImage image)
    lit = MkMIMELiteral (MkMIMEContentType "image" "png" []) $ toStrict bs
    idata = fromJust $ pngDataFromLiteral lit
    in mkDataLiteral lit idata

pngMetadata :: LangPNGImage -> LangHasMetadata
pngMetadata image = keyMapToMetadata $ fst $ idlData image

pngStuff :: BindDocTree ()
pngStuff =
    headingBDT
        "PNG"
        ""
        [ typeBDT "PNGImage" "An image in PNG format." (MkSomeGroundType pngImageGroundType) []
        , hasSubtypeRelationBDT @LangPNGImage @LangImage Verify "" $
          functionToShim "pngImage" $ MkLangImage . mapSome toPixelType . snd . idlData
        , hasSubtypeRelationBDT @LangPNGImage @Literal Verify "" $ functionToShim "pngLiteral" idlLiteral
        , hasSubtypeRelationBDT @LangPNGImage @LangHasMetadata Verify "" $ functionToShim "pngMetadata" pngMetadata
        , valBDT "pngEncode" "Encode an image as PNG, with given metadata." pngEncode
        ]
