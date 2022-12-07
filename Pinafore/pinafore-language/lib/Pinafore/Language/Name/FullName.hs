module Pinafore.Language.Name.FullName where

import Pinafore.Language.Name.Name
import Pinafore.Language.Name.Namespace
import Pinafore.Language.Name.ToText
import Shapes

data FullName =
    MkFullName Namespace
               Name
    deriving (Eq, Ord)

pattern RootFullName :: Name -> FullName

pattern RootFullName n = MkFullName RootNamespace n

fullNameToRoot :: FullName -> Maybe Name
fullNameToRoot (RootFullName n) = Just n
fullNameToRoot _ = Nothing

instance ToText FullName where
    toText (MkFullName RootNamespace name)
        | nameIsInfix name = toText name
    toText (MkFullName nsn name) = toText nsn <> toText name

instance Show FullName where
    show = unpack . toText

instance IsString FullName where
    fromString s =
        fromMaybe (error $ "bad FullName: " <> s) $
        fmap RootFullName (infixNameFromString s) <|> do
            nt <- nonEmpty $ splitSeq "." s
            nspace <- namespaceFromStrings $ init nt
            name <- nameFromString $ last nt
            return $ MkFullName nspace name
