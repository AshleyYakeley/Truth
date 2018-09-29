module Pinafore.Language.Name where

import Shapes

newtype Name =
    MkName Text
    deriving (Eq, MonoFoldable)

instance Show Name where
    show (MkName t) = unpack t

instance IsString Name where
    fromString s = MkName $ fromString s

type instance Element Name = Char
