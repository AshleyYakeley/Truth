module Pinafore.Language.Name.Namespace where

import Pinafore.Language.Name.Name
import Pinafore.Language.Name.ToText
import Shapes

newtype Namespace =
    MkNamespace [Name]
    deriving (Eq, Ord)

instance ToText Namespace where
    toText (MkNamespace nn) = "." <> mconcat (fmap (\t -> toText t <> ".") nn)

instance Show Namespace where
    show = unpack . toText

namespaceFromStrings :: [String] -> Maybe Namespace
namespaceFromStrings ss = do
    let
        ss1 =
            case ss of
                "":ssr -> ssr
                _ -> ss
    ns <-
        for ss1 $ \s -> do
            n <- nameFromString s
            if nameIsUpper n
                then return n
                else Nothing
    return $ MkNamespace ns

instance IsString Namespace where
    fromString s = fromMaybe (error $ "bad Namespace: " <> s) $ namespaceFromStrings $ splitSeq "." s

pattern RootNamespace :: Namespace

pattern RootNamespace = MkNamespace []

namespaceConcat :: Namespace -> [Name] -> Namespace
namespaceConcat (MkNamespace na) nb = MkNamespace $ na <> nb

namespaceStartsWith :: Namespace -> Namespace -> Maybe [Name]
namespaceStartsWith (MkNamespace na) (MkNamespace nb) = startsWith na nb
