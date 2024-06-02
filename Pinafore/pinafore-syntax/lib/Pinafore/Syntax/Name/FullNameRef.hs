module Pinafore.Syntax.Name.FullNameRef where

import Pinafore.Base
import Pinafore.Syntax.Name.FullName
import Pinafore.Syntax.Name.Name
import Pinafore.Syntax.Name.Namespace
import Pinafore.Syntax.Name.NamespaceRef
import Shapes

data FullNameRef = MkFullNameRef
    { fnrName :: Name
    , fnrSpace :: NamespaceRef
    } deriving (Eq, Ord)

pattern UnqualifiedFullNameRef :: Name -> FullNameRef

pattern UnqualifiedFullNameRef n =
        MkFullNameRef n CurrentNamespaceRef

fullNameRefToUnqualified :: FullNameRef -> Maybe Name
fullNameRefToUnqualified (UnqualifiedFullNameRef n) = Just n
fullNameRefToUnqualified _ = Nothing

instance ShowText FullNameRef where
    showText (MkFullNameRef name RootNamespaceRef)
        | nameIsInfix name = showText name
    showText (MkFullNameRef name CurrentNamespaceRef) = showText name
    showText (MkFullNameRef name RootNamespaceRef) = showText name <> "."
    showText (MkFullNameRef name ns) = showText name <> "." <> showText ns

instance Show FullNameRef where
    show = unpack . showText

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
