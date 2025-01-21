module Pinafore.Syntax.Name.Namespace where

import Pinafore.Base
import Shapes

import Pinafore.Syntax.Name.Name

newtype Namespace
    = MkNamespace [Name]
    deriving newtype Eq

instance Ord Namespace where
    compare (MkNamespace a) (MkNamespace b) = compare (reverse a) (reverse b)

instance ShowText Namespace where
    showText (MkNamespace nn) = (intercalate "." $ fmap showText nn) <> "."

instance Show Namespace where
    show = unpack . showText

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

namespaceAppend :: [Name] -> Namespace -> Namespace
namespaceAppend na (MkNamespace nb) = MkNamespace $ na <> nb

namespaceWithin :: Namespace -> Namespace -> Maybe [Name]
namespaceWithin (MkNamespace na) (MkNamespace nb) = endsWith na nb

namespaceParent :: Namespace -> Maybe (Namespace, Name)
namespaceParent (MkNamespace nn) =
    case nn of
        [] -> Nothing
        n : nr -> Just (MkNamespace nr, n)

namespaceAncestry :: Namespace -> NonEmpty Namespace
namespaceAncestry ns =
    ns
        :| case namespaceParent ns of
            Nothing -> []
            Just (nn, _) -> toList $ namespaceAncestry nn
