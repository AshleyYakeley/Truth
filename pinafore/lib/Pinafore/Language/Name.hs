module Pinafore.Language.Name where

import Language.Expression.Common
import Shapes

newtype Name = MkName
    { unName :: Text
    } deriving (Eq, Ord, MonoFoldable)

instance Show Name where
    show = unpack

instance IsString Name where
    fromString s = MkName $ fromString s

type instance Element Name = Char

nameToSymbolType :: Name -> (forall (symbol :: Symbol). SymbolType symbol -> r) -> r
nameToSymbolType n = newUVar $ unpack n

symbolTypeToName :: SymbolType symbol -> Name
symbolTypeToName = MkName . pack . uVarName

newtype ModuleName =
    MkModuleName (NonEmpty Name)
    deriving (Eq, Ord)

instance Show ModuleName where
    show (MkModuleName nn) = intercalate "." $ toList $ fmap unpack nn
