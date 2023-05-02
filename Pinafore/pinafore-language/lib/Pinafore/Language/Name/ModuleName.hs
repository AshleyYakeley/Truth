module Pinafore.Language.Name.ModuleName where

import Pinafore.Text
import Shapes

newtype ModuleName =
    MkModuleName Text
    deriving (Eq, Ord)

instance ToText ModuleName where
    toText (MkModuleName t) = t

instance Show ModuleName where
    show = unpack . toText

instance IsString ModuleName where
    fromString s = MkModuleName $ fromString s

builtInModuleName :: ModuleName
builtInModuleName = MkModuleName "pinafore"
