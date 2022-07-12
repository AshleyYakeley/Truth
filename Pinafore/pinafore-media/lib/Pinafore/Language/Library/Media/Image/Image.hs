module Pinafore.Language.Library.Media.Image.Image where

import Data.Media.Image
import Pinafore.Base
import Pinafore.Language.API
import Shapes

-- LangImage
newtype LangImage = MkLangImage
    { _unLangImage :: SomeFor Image PixelType
    }

imageGroundType :: PinaforeGroundType '[] LangImage
imageGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangImage)|]) "Image"

instance HasPinaforeGroundType '[] LangImage where
    pinaforeGroundType = imageGroundType

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
