module Language.Expression.Name where

import Shapes

newtype Name =
    MkName Text
    deriving (Eq)

instance Show Name where
    show (MkName t) = unpack t

instance IsString Name where
    fromString s = MkName $ fromString s
