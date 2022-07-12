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

pngImageGroundType :: PinaforeGroundType '[] LangPNGImage
pngImageGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangPNGImage)|]) "PNGImage")
        { pgtGreatestDynamicSupertype =
              SimplePolyGreatestDynamicSupertype
                  pinaforeGroundType
                  (functionToShim "fromLiteral" pngFromLiteral)
                  (functionToShim "pngLiteral" idlLiteral)
        }

instance HasPinaforeGroundType '[] LangPNGImage where
    pinaforeGroundType = pngImageGroundType

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

pngStuff :: DocTreeEntry BindDoc
pngStuff =
    docTreeEntry
        "PNG"
        ""
        [ mkTypeEntry "PNGImage" "An image in PNG format." $ MkBoundType pngImageGroundType
        , hasSubtypeRelationEntry @LangPNGImage @LangImage "" $
          functionToShim "pngImage" $ MkLangImage . mapSome toPixelType . snd . idlData
        , hasSubtypeRelationEntry @LangPNGImage @Literal "" $ functionToShim "pngLiteral" idlLiteral
        , hasSubtypeRelationEntry @LangPNGImage @LangHasMetadata "" $ functionToShim "pngMetadata" pngMetadata
        , mkValEntry "pngEncode" "Encode an image as PNG, with given metadata." pngEncode
        ]
