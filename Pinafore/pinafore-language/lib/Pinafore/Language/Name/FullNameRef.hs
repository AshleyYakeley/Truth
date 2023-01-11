module Pinafore.Language.Name.FullNameRef where

import Pinafore.Language.Name.FullName
import Pinafore.Language.Name.Name
import Pinafore.Language.Name.Namespace
import Pinafore.Language.Name.NamespaceRef
import Pinafore.Language.Name.ToText
import Shapes

data FullNameRef = MkFullNameRef
    { fnName :: Name
    , fnSpace :: NamespaceRef
    } deriving (Eq, Ord)

pattern UnqualifiedFullNameRef :: Name -> FullNameRef

pattern UnqualifiedFullNameRef n =
        MkFullNameRef n CurrentNamespaceRef

fullNameRefToUnqualified :: FullNameRef -> Maybe Name
fullNameRefToUnqualified (UnqualifiedFullNameRef n) = Just n
fullNameRefToUnqualified _ = Nothing

instance ToText FullNameRef where
    toText (MkFullNameRef name RootNamespaceRef)
        | nameIsInfix name = toText name
    toText (MkFullNameRef name CurrentNamespaceRef) = toText name
    toText (MkFullNameRef name RootNamespaceRef) = toText name <> "."
    toText (MkFullNameRef name ns) = toText name <> "." <> toText ns

instance Show FullNameRef where
    show = unpack . toText

instance IsString FullNameRef where
    fromString "." = MkFullNameRef "." CurrentNamespaceRef
    fromString s =
        case nonEmpty s of
            Just ns
                | '.' <- last ns -> fullNameRef $ fromString s
            _ ->
                case fromString s of
                    MkFullName n (MkNamespace ns) -> MkFullNameRef n (RelativeNamespaceRef ns)

namespaceConcatFullName :: Namespace -> FullNameRef -> FullName
namespaceConcatFullName ns (MkFullNameRef name nref) = MkFullName name (namespaceConcatRef ns nref)

fullNameRef :: FullName -> FullNameRef
fullNameRef (MkFullName name ns) = MkFullNameRef name (AbsoluteNamespaceRef ns)

fullNameRootRelative :: FullName -> FullNameRef
fullNameRootRelative (MkFullName n ns) = MkFullNameRef n (namespaceRootRelative ns)

-- | All the ways a 'FullName' can be split into a 'Namespace' and 'FullNameRef', starting with the longest 'Namespace' and shortest 'FullNameRef'.
fullNameSplits :: FullName -> [(Namespace, FullNameRef)]
fullNameSplits (MkFullName name ns) = fmap (fmap $ \nsr -> MkFullNameRef name nsr) $ namespaceSplits ns

namespaceWithinFullNameRef :: Namespace -> FullName -> Maybe FullNameRef
namespaceWithinFullNameRef na (MkFullName n nb) = fmap (MkFullNameRef n) $ namespaceWithinRef na nb

namespaceRelativeFullName :: Namespace -> FullName -> FullNameRef
namespaceRelativeFullName na (MkFullName n nb) = MkFullNameRef n $ namespaceRelative na nb

relativeNamespace :: [Namespace] -> Namespace -> NamespaceRef
relativeNamespace basens fn =
    fromMaybe (namespaceRootRelative fn) $
    choice $ fmap (\(ns, fref) -> ifpure (elem ns basens) fref) $ namespaceSplits fn

relativeFullName :: [Namespace] -> FullName -> FullNameRef
relativeFullName basens fn =
    fromMaybe (fullNameRootRelative fn) $
    choice $ fmap (\(ns, fref) -> ifpure (elem ns basens) fref) $ fullNameSplits fn
