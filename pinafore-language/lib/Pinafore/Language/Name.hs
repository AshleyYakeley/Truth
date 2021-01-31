module Pinafore.Language.Name where

import Language.Expression.Common
import Shapes

class ToText t where
    toText :: t -> Text

newtype Name =
    MkName Text
    deriving (Eq, Ord, MonoFoldable)

instance ToText Name where
    toText (MkName n) = n

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

instance ToText ModuleName where
    toText (MkModuleName nn) = intercalate "." $ fmap toText $ toList nn

instance Show ModuleName where
    show = unpack . toText

data ReferenceName
    = QualifiedReferenceName ModuleName
                             Name
    | UnqualifiedReferenceName Name

instance ToText ReferenceName where
    toText (UnqualifiedReferenceName n) = toText n
    toText (QualifiedReferenceName m n) = toText m <> "." <> toText n

instance Show ReferenceName where
    show = unpack . toText
