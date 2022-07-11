module Pinafore.Language.Library.Media.Image.PNG
    ( pngStuff
    ) where

import Changes.World.MIME
import Data.Media.Image
import Data.Shim
import Pinafore.Base
import Pinafore.Language.API
import Pinafore.Language.Library.Media.Image.Image
import Shapes hiding (rotate)

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

pngEncode :: Maybe (Integer, Integer) -> LangImage -> LangPNGImage
pngEncode mdpi (MkLangImage image) = let
    mdata =
        case mdpi of
            Nothing -> mempty
            Just (dx, dy) -> witnessMapOfFromList $ [MkSomeOf DpiX $ fromInteger dx, MkSomeOf DpiY $ fromInteger dy]
    bs = encode pngFormat (mdata, someConvertImage image)
    lit = MkMIMELiteral (MkMIMEContentType "image" "png" []) $ toStrict bs
    idata = fromJust $ pngDataFromLiteral lit
    in mkDataLiteral lit idata

pngDPI :: LangPNGImage -> Maybe (Int, Int)
pngDPI image = let
    mdata = fst $ idlData image
    in do
           dx <- witnessMapOfLookup DpiX mdata
           dy <- witnessMapOfLookup DpiY mdata
           return (fromIntegral dx, fromIntegral dy)

pngStuff :: DocTreeEntry BindDoc
pngStuff =
    docTreeEntry
        "PNG"
        ""
        [ mkTypeEntry "PNGImage" "An image in PNG format." $ MkBoundType pngImageGroundType
        , hasSubtypeRelationEntry @LangPNGImage @LangImage "" $
          functionToShim "pngImage" $ MkLangImage . mapSome toPixelType . snd . idlData
        , hasSubtypeRelationEntry @LangPNGImage @Literal "" $ functionToShim "pngLiteral" idlLiteral
        , mkValEntry "pngEncode" "Encode an image as PNG, with given resolution (in dpi)." pngEncode
        , mkValEntry "pngDPI" "The resolution of an image (in dpi), if available." pngDPI
        ]
