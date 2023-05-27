module Pinafore.Language.Name.Namespace where

import Pinafore.Language.Name.Name
import Pinafore.Text
import Shapes

newtype Namespace =
    MkNamespace [Name]
    deriving (Eq)

instance Ord Namespace where
    compare (MkNamespace a) (MkNamespace b) = compare (reverse a) (reverse b)

instance ToText Namespace where
    toText (MkNamespace nn) = (intercalate "." $ fmap toText nn) <> "."

instance Show Namespace where
    show = unpack . toText

namespaceFromStrings :: [String] -> Maybe Namespace
namespaceFromStrings ss = do
    let
        ss1 =
            case nonEmpty ss of
                Just ("" :| [""]) -> []
                Just ssn
                    | "" <- last ssn -> init ssn
                _ -> ss
    ns <- for ss1 upperNameFromString
    return $ MkNamespace ns

instance IsString Namespace where
    fromString s = fromMaybe (error $ "bad Namespace: " <> show s) $ namespaceFromStrings $ splitSeq "." s

pattern RootNamespace :: Namespace

pattern RootNamespace = MkNamespace []

namespaceAppend :: Namespace -> [Name] -> Namespace
namespaceAppend (MkNamespace na) nb = MkNamespace $ nb <> na

namespaceWithin :: Namespace -> Namespace -> Maybe [Name]
namespaceWithin (MkNamespace na) (MkNamespace nb) = endsWith na nb

namespaceParent :: Namespace -> Maybe (Namespace, Name)
namespaceParent (MkNamespace nn) =
    case nn of
        [] -> Nothing
        n:nr -> Just (MkNamespace nr, n)

namespaceAncestry :: Namespace -> NonEmpty Namespace
namespaceAncestry ns =
    ns :|
    case namespaceParent ns of
        Nothing -> []
        Just (nn, _) -> toList $ namespaceAncestry nn
