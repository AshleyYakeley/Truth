module Pinafore.Language.Name.FullName where

import Pinafore.Language.Name.Name
import Pinafore.Language.Name.Namespace
import Pinafore.Text
import Shapes

data FullName = MkFullName
    { fnName :: Name
    , fnSpace :: Namespace
    } deriving (Eq, Ord)

pattern RootFullName :: Name -> FullName

pattern RootFullName n = MkFullName n RootNamespace

-- | The companion namespace of N is a namespace with the same name as N.
fnCompanion :: FullName -> Namespace
fnCompanion (MkFullName name ns) = namespaceAppend ns [name]

fullNameToRoot :: FullName -> Maybe Name
fullNameToRoot (RootFullName n) = Just n
fullNameToRoot _ = Nothing

instance ShowText FullName where
    showText (MkFullName name RootNamespace)
        | nameIsInfix name = showText name
    showText (MkFullName name RootNamespace) = showText name <> "."
    showText (MkFullName name nsn) = showText name <> "." <> showText nsn

instance Show FullName where
    show = unpack . showText

instance IsString FullName where
    fromString s =
        fromMaybe (error $ "bad FullName: " <> show s) $
        fmap RootFullName (infixNameFromString s) <|> do
            nt <- nonEmpty $ splitSeq "." s
            case nt of
                "" :| "":nss -> do
                    nspace <- namespaceFromStrings nss
                    return $ MkFullName "." nspace
                _ -> do
                    name <- nameFromString $ head nt
                    nspace <- namespaceFromStrings $ tail nt
                    return $ MkFullName name nspace
