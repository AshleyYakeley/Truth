module Pinafore.Language.Library.Media.Image
    ( imageLibraryModule
    , LangJPEGImage(..)
    ) where

import Changes.World.MIME
import Data.Media.Image
import Data.Shim
import Pinafore.Base
import Pinafore.Language.API
import Pinafore.Language.Library.Media.Colour
import Shapes hiding (rotate)

-- LangImage
newtype LangImage = MkLangImage
    { _unLangImage :: SomeFor Image PixelType
    }

imageGroundType :: PinaforeGroundType '[] LangImage
imageGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangImage)|]) "Image"

instance HasPinaforeGroundType '[] LangImage where
    pinaforeGroundType = imageGroundType

langImageSize :: LangImage -> (Int, Int)
langImageSize (MkLangImage (MkSomeFor _ image)) = imageSize image

data DataLiteral t = MkDataLiteral
    { dlLiteral :: Literal
    , dlData :: t
    }

class IsDataLiteral t dl | dl -> t where
    mkDataLiteral :: Literal -> t -> dl
    idlLiteral :: dl -> Literal
    idlData :: dl -> t

instance IsDataLiteral t (DataLiteral t) where
    mkDataLiteral = MkDataLiteral
    idlLiteral = dlLiteral
    idlData = dlData

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

colourToPixel :: LangColour -> PixelRGB16
colourToPixel (ColorSRGB r g b) = PixelRGB16 r g b

alphaColourToPixel :: LangAlphaColour -> PixelRGBA16
alphaColourToPixel (Alpha (ColorSRGB r g b) a) = PixelRGBA16 r g b a

langBlankImage :: LangAlphaColour -> (Int, Int) -> LangImage
langBlankImage (MkOpaqueAlphaColour col) size =
    MkLangImage $ MkSomeFor RGB16PixelType $ blankImage size $ colourToPixel col
langBlankImage acol size = MkLangImage $ MkSomeFor RGBA16PixelType $ blankImage size $ alphaColourToPixel acol

jpegDPI :: LangJPEGImage -> Maybe (Int, Int)
jpegDPI image = let
    mdata = fst $ idlData image
    in do
           dx <- witnessMapOfLookup DpiX mdata
           dy <- witnessMapOfLookup DpiY mdata
           return (fromIntegral dx, fromIntegral dy)

imageLibraryModule :: LibraryModule
imageLibraryModule =
    MkDocTree
        "Image"
        ""
        [ mkTypeEntry "Image" "An image." $ MkBoundType imageGroundType
        , mkValEntry "imageSize" "The size of an image" langImageSize
        , mkValEntry "blankImage" "An image of one colour" langBlankImage
        , docTreeEntry
              "JPEG"
              ""
              [ mkTypeEntry "JPEGImage" "An image in JPEG format." $ MkBoundType jpegImageGroundType
              , hasSubtypeRelationEntry @LangJPEGImage @LangImage "" $
                functionToShim "jpegImage" $ MkLangImage . mapSome toPixelType . snd . idlData
              , hasSubtypeRelationEntry @LangJPEGImage @Literal "" $ functionToShim "jpegLiteral" idlLiteral
              , mkValEntry
                    "jpegEncode"
                    "Encode an image as JPEG, with given quality and resolution (in dpi)."
                    jpegEncode
              , mkValEntry "jpegDPI" "The resolution of an image (in dpi), if available." jpegDPI
              ]
        ]
