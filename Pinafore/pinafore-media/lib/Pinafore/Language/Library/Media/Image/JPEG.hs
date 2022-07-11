module Pinafore.Language.Library.Media.Image.JPEG
    ( jpegStuff
    ) where

import Changes.World.MIME
import Data.Media.Image
import Data.Shim
import Pinafore.Base
import Pinafore.Language.API
import Pinafore.Language.Library.Media.Image.Image
import Shapes hiding (rotate)

type JPEGData = (WitnessMapOf ImageDataKey, SomeFor Image JPEGPixelType)

-- LangJPEGImage
newtype LangJPEGImage =
    MkLangJPEGImage (DataLiteral JPEGData)
    deriving (IsDataLiteral JPEGData)

jpegImageGroundType :: PinaforeGroundType '[] LangJPEGImage
jpegImageGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangJPEGImage)|]) "JPEGImage")
        { pgtGreatestDynamicSupertype =
              SimplePolyGreatestDynamicSupertype
                  pinaforeGroundType
                  (functionToShim "fromLiteral" jpegFromLiteral)
                  (functionToShim "jpegLiteral" idlLiteral)
        }

instance HasPinaforeGroundType '[] LangJPEGImage where
    pinaforeGroundType = jpegImageGroundType

jpegDataFromLiteral :: Literal -> Maybe JPEGData
jpegDataFromLiteral (MkMIMELiteral (MkMIMEContentType "image" "jpeg" _) bs) = do
    resultToMaybe $ decode (jpegFormat 0) $ fromStrict bs
jpegDataFromLiteral _ = Nothing

jpegFromLiteral :: Literal -> Maybe LangJPEGImage
jpegFromLiteral lit = do
    idata <- jpegDataFromLiteral lit
    return $ mkDataLiteral lit idata

jpegEncode :: Integer -> Maybe (Integer, Integer) -> LangImage -> LangJPEGImage
jpegEncode q mdpi (MkLangImage image) = let
    mdata =
        case mdpi of
            Nothing -> mempty
            Just (dx, dy) -> witnessMapOfFromList $ [MkSomeOf DpiX $ fromInteger dx, MkSomeOf DpiY $ fromInteger dy]
    bs = encode (jpegFormat $ fromInteger q) (mdata, someConvertImage image)
    lit = MkMIMELiteral (MkMIMEContentType "image" "jpeg" []) $ toStrict bs
    idata = fromJust $ jpegDataFromLiteral lit
    in mkDataLiteral lit idata

jpegDPI :: LangJPEGImage -> Maybe (Int, Int)
jpegDPI image = let
    mdata = fst $ idlData image
    in do
           dx <- witnessMapOfLookup DpiX mdata
           dy <- witnessMapOfLookup DpiY mdata
           return (fromIntegral dx, fromIntegral dy)

jpegStuff :: DocTreeEntry BindDoc
jpegStuff =
    docTreeEntry
        "JPEG"
        ""
        [ mkTypeEntry "JPEGImage" "An image in JPEG format." $ MkBoundType jpegImageGroundType
        , hasSubtypeRelationEntry @LangJPEGImage @LangImage "" $
          functionToShim "jpegImage" $ MkLangImage . mapSome toPixelType . snd . idlData
        , hasSubtypeRelationEntry @LangJPEGImage @Literal "" $ functionToShim "jpegLiteral" idlLiteral
        , mkValEntry "jpegEncode" "Encode an image as JPEG, with given quality and resolution (in dpi)." jpegEncode
        , mkValEntry "jpegDPI" "The resolution of an image (in dpi), if available." jpegDPI
        ]
