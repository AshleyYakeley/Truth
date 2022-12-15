module Pinafore.Language.Name.Namespace where

import Pinafore.Language.Name.Name
import Pinafore.Language.Name.ToText
import Shapes

newtype Namespace =
    MkNamespace [Name]
    deriving (Eq, Ord)

instance ToText Namespace where
    toText (MkNamespace nn) = "." <> (intercalate "." $ fmap toText nn)

instance Show Namespace where
    show = unpack . toText

namespaceFromStrings :: [String] -> Maybe Namespace
namespaceFromStrings ss = do
    let
        ss1 =
            case ss of
                ["", ""] -> []
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
    fromString s = fromMaybe (error $ "bad Namespace: " <> show s) $ namespaceFromStrings $ splitSeq "." s

pattern RootNamespace :: Namespace

pattern RootNamespace = MkNamespace []

namespaceStartsWith :: Namespace -> Namespace -> Maybe [Name]
namespaceStartsWith (MkNamespace na) (MkNamespace nb) = startsWith na nb

namespaceAncestry :: Namespace -> [Namespace]
namespaceAncestry a@(MkNamespace nn) =
    a :
    case nonEmpty nn of
        Nothing -> []
        Just na -> namespaceAncestry $ MkNamespace $ init na
