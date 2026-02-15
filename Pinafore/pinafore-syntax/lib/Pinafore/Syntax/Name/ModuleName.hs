module Pinafore.Syntax.Name.ModuleName where

import Shapes

import Pinafore.Syntax.Name.Show.Text

newtype ModuleName
    = MkModuleName Text
    deriving newtype (Eq, Ord)

instance ShowText ModuleName where
    showText (MkModuleName t) = t

instance Show ModuleName where
    show = unpack . showText

instance IsString ModuleName where
    fromString s = MkModuleName $ fromString s

builtInModuleName :: ModuleName
builtInModuleName = MkModuleName "pinafore"
