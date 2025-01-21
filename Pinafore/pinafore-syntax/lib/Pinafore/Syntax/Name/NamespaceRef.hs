module Pinafore.Syntax.Name.NamespaceRef where

import Pinafore.Base
import Shapes

import Pinafore.Syntax.Name.Name
import Pinafore.Syntax.Name.Namespace

data NamespaceRef
    = AbsoluteNamespaceRef Namespace
    | RelativeNamespaceRef [Name]
    deriving stock (Eq, Ord)

pattern CurrentNamespaceRef :: NamespaceRef
pattern CurrentNamespaceRef = RelativeNamespaceRef []

pattern RootNamespaceRef :: NamespaceRef
pattern RootNamespaceRef = AbsoluteNamespaceRef RootNamespace

namespaceConcatRefM :: Applicative m => m Namespace -> NamespaceRef -> m Namespace
namespaceConcatRefM _ (AbsoluteNamespaceRef nsb) = pure nsb
namespaceConcatRefM mnsa (RelativeNamespaceRef namesb) = fmap (namespaceAppend namesb) mnsa

namespaceConcatRef :: Namespace -> NamespaceRef -> Namespace
namespaceConcatRef nsa nsb = runIdentity $ namespaceConcatRefM (Identity nsa) nsb

instance ShowText NamespaceRef where
    showText (RelativeNamespaceRef nn) = intercalate "." $ fmap showText nn
    showText (AbsoluteNamespaceRef asn) = showText asn

instance Show NamespaceRef where
    show = unpack . showText

namespaceRefAppend :: [Name] -> NamespaceRef -> NamespaceRef
namespaceRefAppend na (AbsoluteNamespaceRef ns) = AbsoluteNamespaceRef $ namespaceAppend na ns
namespaceRefAppend na (RelativeNamespaceRef nb) = RelativeNamespaceRef $ na <> nb

namespaceRefFromStrings :: [String] -> Maybe NamespaceRef
namespaceRefFromStrings ss = do
    ssn <- nonEmpty ss
    case ssn of
        "" :| [] -> return CurrentNamespaceRef
        _ ->
            case last ssn of
                "" -> do
                    ns <- for (init ssn) upperNameFromString
                    return $ AbsoluteNamespaceRef $ MkNamespace ns
                _ -> do
                    ns <- for (toList ssn) upperNameFromString
                    return $ RelativeNamespaceRef ns

instance IsString NamespaceRef where
    fromString s = fromMaybe (error $ "bad NamespaceRef: " <> s) $ namespaceRefFromStrings $ splitSeq "." s

namespaceRootRelative :: Namespace -> NamespaceRef
namespaceRootRelative (MkNamespace nn) = RelativeNamespaceRef nn

-- | All the ways a 'Namespace' can be split into a 'Namespace' and relative 'NamespaceRef', starting with the longest 'Namespace' and shortest 'NamespaceRef'.
namespaceSplits :: Namespace -> [(Namespace, NamespaceRef)]
namespaceSplits (MkNamespace ns) = fmap (\(s1, s2) -> (MkNamespace s1, RelativeNamespaceRef s2)) $ splits ns
    where
        splits1 :: forall a. [a] -> [([a], [a])]
        splits1 [] = []
        splits1 (a : aa) = fmap (\(s1, s2) -> (s1, a : s2)) $ splits aa
        splits :: forall a. [a] -> [([a], [a])]
        splits aa = (aa, []) : splits1 aa

namespaceWithinRef :: Namespace -> Namespace -> Maybe NamespaceRef
namespaceWithinRef na nb = fmap RelativeNamespaceRef $ namespaceWithin na nb

namespaceRelative :: Namespace -> Namespace -> NamespaceRef
namespaceRelative na nb = fromMaybe (AbsoluteNamespaceRef nb) $ namespaceWithinRef na nb
