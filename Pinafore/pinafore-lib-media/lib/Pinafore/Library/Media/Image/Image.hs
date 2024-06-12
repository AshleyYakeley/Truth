module Pinafore.Library.Media.Image.Image where

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
