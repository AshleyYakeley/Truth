module Pinafore.Language.Name.Name where

import Language.Expression.Common
import Pinafore.Language.Name.ToText
import Shapes

newtype Name =
    MkName Text
    deriving (Eq, Ord, MonoFoldable)

instance ToText Name where
    toText (MkName n) = n

instance Show Name where
    show = unpack

instance IsString Name where
    fromString s = fromMaybe (error "bad Name: empty") $ nameFromString s

nameFromString :: String -> Maybe Name
nameFromString "" = Nothing
nameFromString s = Just $ MkName $ fromString s

nameIsUpper :: Name -> Bool
nameIsUpper (MkName n) =
    case headMay n of
        Just c -> isUpper c
        Nothing -> False

nameIsInfix :: Name -> Bool
nameIsInfix (MkName n) =
    case headMay n of
        Just c -> not $ isAlpha c
        Nothing -> False

infixNameFromString :: String -> Maybe Name
infixNameFromString s = do
    name <- nameFromString s
    if nameIsInfix name
        then Just name
        else Nothing

type instance Element Name = Char

nameToSymbolType :: Name -> (forall (symbol :: Symbol). SymbolType symbol -> r) -> r
nameToSymbolType n = newUVar $ unpack n

symbolTypeToName :: SymbolType symbol -> Name
symbolTypeToName = MkName . pack . uVarName
