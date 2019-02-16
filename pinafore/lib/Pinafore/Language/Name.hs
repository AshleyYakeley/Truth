module Pinafore.Language.Name where

import Shapes

newtype Name = MkName
    { unName :: Text
    } deriving (Eq, Ord, MonoFoldable)

instance Show Name where
    show = unpack

instance IsString Name where
    fromString s = MkName $ fromString s

type instance Element Name = Char

nameToSymbolWitness :: Name -> (forall (symbol :: Symbol). SymbolType symbol -> r) -> r
nameToSymbolWitness n = toSymbolType $ unpack n
