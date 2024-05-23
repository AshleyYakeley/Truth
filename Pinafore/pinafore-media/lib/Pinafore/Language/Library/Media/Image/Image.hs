module Pinafore.Language.Library.Media.Image.Image where

import Changes.World.MIME
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

class DecodeMIME t where
    dmMatchContentType :: MIMEContentType -> Bool
    dmDecode :: StrictByteString -> Maybe t
    dmLiteralContentType :: MIMEContentType

dmFromMIME ::
       forall t. DecodeMIME t
    => MIME
    -> Maybe t
dmFromMIME (MkMIME mt bs)
    | dmMatchContentType @t mt = dmDecode bs
dmFromMIME _ = Nothing

dmFromLiteral ::
       forall t. DecodeMIME t
    => Literal
    -> Maybe t
dmFromLiteral (MkMIMELiteral m) = dmFromMIME m
dmFromLiteral _ = Nothing

data DataLiteral t = MkDataLiteral
    { dlLiteral :: Literal
    , dlData :: t
    }

class (DecodeMIME (LiteralData dl)) => IsDataLiteral (dl :: Type) where
    type LiteralData dl :: Type
    mkDataLiteral :: Literal -> LiteralData dl -> dl
    idlLiteral :: dl -> Literal
    idlData :: dl -> LiteralData dl

instance DecodeMIME t => IsDataLiteral (DataLiteral t) where
    type LiteralData (DataLiteral t) = t
    mkDataLiteral = MkDataLiteral
    idlLiteral = dlLiteral
    idlData = dlData

idlMIME :: IsDataLiteral dl => dl -> Maybe MIME
idlMIME = literalToMIME . idlLiteral

dmMkDataLiteral ::
       forall dl. IsDataLiteral dl
    => StrictByteString
    -> LiteralData dl
    -> dl
dmMkDataLiteral bs idata = let
    lit = MkMIMELiteral $ MkMIME (dmLiteralContentType @(LiteralData dl)) bs
    in mkDataLiteral lit idata

mimeToDataLiteral ::
       forall dl. IsDataLiteral dl
    => MIME
    -> Maybe dl
mimeToDataLiteral m = do
    idata <- dmFromMIME m
    return $ dmMkDataLiteral (mimeContent m) idata

dataLiteralMIMEPrism ::
       forall dl. IsDataLiteral dl
    => LangPrism' MIME dl
dataLiteralMIMEPrism = prism mimeToDataLiteral $ fromJust . idlMIME

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
