module Pinafore.Language.Name.FullNameRef where

import Pinafore.Language.Name.FullName
import Pinafore.Language.Name.Name
import Pinafore.Language.Name.Namespace
import Pinafore.Language.Name.NamespaceRef
import Pinafore.Language.Name.ToText
import Shapes

data FullNameRef = MkFullNameRef
    { fnSpace :: NamespaceRef
    , fnName :: Name
    } deriving (Eq, Ord)

pattern UnqualifiedFullNameRef :: Name -> FullNameRef

pattern UnqualifiedFullNameRef n =
        MkFullNameRef CurrentNamespaceRef n

fullNameRefToUnqualified :: FullNameRef -> Maybe Name
fullNameRefToUnqualified (UnqualifiedFullNameRef n) = Just n
fullNameRefToUnqualified _ = Nothing

instance ToText FullNameRef where
    toText (MkFullNameRef RootNamespaceRef name)
        | nameIsInfix name = toText name
    toText (MkFullNameRef CurrentNamespaceRef name) = toText name
    toText (MkFullNameRef RootNamespaceRef name) = "." <> toText name
    toText (MkFullNameRef ns name) = toText ns <> "." <> toText name

instance Show FullNameRef where
    show = unpack . toText

instance IsString FullNameRef where
    fromString s@('.':_) = fullNameRef $ fromString s
    fromString s =
        case fromString s of
            MkFullName (MkNamespace ns) n -> MkFullNameRef (RelativeNamespaceRef ns) n

fullNameRefInNamespace :: Namespace -> FullNameRef -> FullName
fullNameRefInNamespace ns (MkFullNameRef nref name) = MkFullName (namespaceRefInNamespace ns nref) name

fullNameRef :: FullName -> FullNameRef
fullNameRef (MkFullName ns name) = MkFullNameRef (AbsoluteNamespaceRef ns) name

fullNameRelFromRoot :: FullName -> FullNameRef
fullNameRelFromRoot (MkFullName ns n) = MkFullNameRef (namespaceRelFromRoot ns) n

-- | All the ways a 'FullName' can be split into a 'Namespace' and 'FullNameRef', starting with the longest 'Namespace' and shortest 'FullNameRef'.
fullNameSplits :: FullName -> [(Namespace, FullNameRef)]
fullNameSplits (MkFullName ns name) = fmap (fmap $ \nsr -> MkFullNameRef nsr name) $ namespaceSplits ns

relativeFullName :: [Namespace] -> FullName -> FullNameRef
relativeFullName basens fn =
    fromMaybe (fullNameRelFromRoot fn) $ choice $ fmap (\(ns, fref) -> ifpure (elem ns basens) fref) $ fullNameSplits fn
