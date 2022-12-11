module Pinafore.Language.Name.NamespaceRef where

import Pinafore.Language.Name.Name
import Pinafore.Language.Name.Namespace
import Pinafore.Language.Name.ToText
import Shapes

data NamespaceRef
    = AbsoluteNamespaceRef Namespace
    | RelativeNamespaceRef [Name]
    deriving (Eq, Ord)

pattern CurrentNamespaceRef :: NamespaceRef

pattern CurrentNamespaceRef = RelativeNamespaceRef []

pattern RootNamespaceRef :: NamespaceRef

pattern RootNamespaceRef = AbsoluteNamespaceRef RootNamespace

namespaceRefInNamespace :: Namespace -> NamespaceRef -> Namespace
namespaceRefInNamespace _ (AbsoluteNamespaceRef ns) = ns
namespaceRefInNamespace ns (RelativeNamespaceRef names) = namespaceConcat ns names

instance ToText NamespaceRef where
    toText (RelativeNamespaceRef nn) = intercalate "." $ fmap toText nn
    toText (AbsoluteNamespaceRef asn) = toText asn

instance Show NamespaceRef where
    show = unpack . toText

namespaceRefFromStrings :: [String] -> Maybe NamespaceRef
namespaceRefFromStrings [""] = return CurrentNamespaceRef
namespaceRefFromStrings ("":ss) = do
    ns <-
        for ss $ \s -> do
            n <- nameFromString s
            if nameIsUpper n
                then return n
                else Nothing
    return $ AbsoluteNamespaceRef $ MkNamespace ns
namespaceRefFromStrings ss = do
    ns <-
        for ss $ \s -> do
            n <- nameFromString s
            if nameIsUpper n
                then return n
                else Nothing
    return $ RelativeNamespaceRef ns

instance IsString NamespaceRef where
    fromString s = fromMaybe (error $ "bad NamespaceRef: " <> s) $ namespaceRefFromStrings $ splitSeq "." s

namespaceRelFromRoot :: Namespace -> NamespaceRef
namespaceRelFromRoot (MkNamespace nn) = RelativeNamespaceRef nn

-- | All the ways a 'Namespace' can be split into a 'Namespace' and relative 'NamespaceRef', starting with the longest 'Namespace' and shortest 'NamespaceRef'.
namespaceSplits :: Namespace -> [(Namespace, NamespaceRef)]
namespaceSplits (MkNamespace ns) = fmap (\(s1, s2) -> (MkNamespace s1, RelativeNamespaceRef s2)) $ splits ns
  where
    splits :: forall a. [a] -> [([a], [a])]
    splits [] = [([], [])]
    splits (a:aa) = let
        ss = splits aa
        in fmap (\(s1, s2) -> (a : s1, s2)) ss <> [([], a : aa)]
