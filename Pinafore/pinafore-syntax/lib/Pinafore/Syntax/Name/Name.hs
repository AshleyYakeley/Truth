module Pinafore.Syntax.Name.Name where

import Language.Expression.Common
import Pinafore.Base
import Shapes

allowedAlphaNameFirstChar :: Char -> Bool
allowedAlphaNameFirstChar '_' = True
allowedAlphaNameFirstChar c = isAlpha c

allowedAlphaNameChar :: Char -> Bool
allowedAlphaNameChar '-' = True
allowedAlphaNameChar '_' = True
allowedAlphaNameChar c = isAlphaNum c

newtype Name =
    MkName Text
    deriving (Eq, Ord, MonoFoldable)

instance ShowText Name where
    showText (MkName n) = n

instance Show Name where
    show = unpack

instance IsString Name where
    fromString s = fromMaybe (error $ "bad Name: " <> show s) $ nameFromString s

nameFromString :: String -> Maybe Name
nameFromString "" = Nothing
nameFromString s = Just $ MkName $ fromString s

nameIsUpper :: Name -> Bool
nameIsUpper (MkName n) =
    oall allowedAlphaNameChar n &&
    case headMay n of
        Just c -> isUpper c
        Nothing -> False

upperNameFromString :: String -> Maybe Name
upperNameFromString s = do
    name <- nameFromString s
    if nameIsUpper name
        then Just name
        else Nothing

nameIsInfix :: Name -> Bool
nameIsInfix (MkName "") = False
nameIsInfix (MkName n) = not $ any isAlpha $ unpack n

infixNameFromString :: String -> Maybe Name
infixNameFromString s = do
    name <- nameFromString s
    if nameIsInfix name
        then Just name
        else Nothing

type instance Element Name = Char

nameToTypeVarT :: Name -> (forall tv. TypeVarT tv -> r) -> r
nameToTypeVarT n = newTypeVar $ unpack n

typeVarToName :: TypeVar tv -> Name
typeVarToName = MkName . pack . typeVarName

someTypeVarToName :: SomeTypeVarT -> Name
someTypeVarToName (MkSomeTypeVarT v) = typeVarToName v
