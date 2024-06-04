module Pinafore.Language.Library.Media.Image.Image where

import Changes.World.Media.Type
import Data.Media.Image
import Pinafore.API
import Shapes

-- LangImage
newtype LangImage = MkLangImage
    { unLangImage :: SomeFor Image PixelType
    }

imageGroundType :: QGroundType '[] LangImage
imageGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangImage)|]) "Image"

instance HasQGroundType '[] LangImage where
    qGroundType = imageGroundType

class DecodeMedia t where
    dmMatchContentType :: MediaType -> Bool
    dmDecode :: StrictByteString -> Maybe t
    dmLiteralContentType :: MediaType

dmFromMedia ::
       forall t. DecodeMedia t
    => Media
    -> Maybe t
dmFromMedia (MkMedia mt bs)
    | dmMatchContentType @t mt = dmDecode bs
dmFromMedia _ = Nothing

dmFromLiteral ::
       forall t. DecodeMedia t
    => Literal
    -> Maybe t
dmFromLiteral (MkMediaLiteral m) = dmFromMedia m
dmFromLiteral _ = Nothing

data DataLiteral t = MkDataLiteral
    { dlLiteral :: Literal
    , dlData :: t
    }

class (DecodeMedia (LiteralData dl)) => IsDataLiteral (dl :: Type) where
    type LiteralData dl :: Type
    mkDataLiteral :: Literal -> LiteralData dl -> dl
    idlLiteral :: dl -> Literal
    idlData :: dl -> LiteralData dl

instance DecodeMedia t => IsDataLiteral (DataLiteral t) where
    type LiteralData (DataLiteral t) = t
    mkDataLiteral = MkDataLiteral
    idlLiteral = dlLiteral
    idlData = dlData

idlMedia :: IsDataLiteral dl => dl -> Maybe Media
idlMedia = literalToMedia . idlLiteral

dmMkDataLiteral ::
       forall dl. IsDataLiteral dl
    => StrictByteString
    -> LiteralData dl
    -> dl
dmMkDataLiteral bs idata = let
    lit = MkMediaLiteral $ MkMedia (dmLiteralContentType @(LiteralData dl)) bs
    in mkDataLiteral lit idata

mediaToDataLiteral ::
       forall dl. IsDataLiteral dl
    => Media
    -> Maybe dl
mediaToDataLiteral m = do
    idata <- dmFromMedia m
    return $ dmMkDataLiteral (mediaContent m) idata

dataLiteralMediaPrism ::
       forall dl. IsDataLiteral dl
    => LangPrism' Media dl
dataLiteralMediaPrism = prism mediaToDataLiteral $ fromJust . idlMedia

literalToDataLiteral ::
       forall dl. IsDataLiteral dl
    => Literal
    -> Maybe dl
literalToDataLiteral lit = do
    idata <- dmFromLiteral lit
    return $ mkDataLiteral lit idata

bytesToDataLiteral ::
       forall dl. IsDataLiteral dl
    => StrictByteString
    -> dl
bytesToDataLiteral bs = dmMkDataLiteral bs $ fromJust $ dmDecode bs
