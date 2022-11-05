module Pinafore.Language.Name where

import Language.Expression.Common
import Shapes

class ToText t where
    toText :: t -> Text

newtype Name =
    MkName Text
    deriving (Eq, Ord, MonoFoldable)

instance ToText Name where
    toText (MkName n) = n

instance Show Name where
    show = unpack

instance IsString Name where
    fromString s = fromMaybe (error "bad Name (empty)") $ nameFromString s

nameFromString :: String -> Maybe Name
nameFromString "" = Nothing
nameFromString s = Just $ MkName $ fromString s

nameIsUpper :: Name -> Bool
nameIsUpper n =
    case unpack n of
        c:_ -> isUpper c
        _ -> False

infixNameFromString :: String -> Maybe Name
infixNameFromString s =
    case s of
        c:_
            | isAlpha c -> Nothing
        "[]" -> Nothing
        _ -> nameFromString s

nameIsInfix :: Name -> Bool
nameIsInfix n = isJust $ infixNameFromString $ unpack n

type instance Element Name = Char

nameToSymbolType :: Name -> (forall (symbol :: Symbol). SymbolType symbol -> r) -> r
nameToSymbolType n = newUVar $ unpack n

symbolTypeToName :: SymbolType symbol -> Name
symbolTypeToName = MkName . pack . uVarName

newtype Namespace =
    MkNamespace [Name]
    deriving (Eq, Ord)

instance ToText Namespace where
    toText (MkNamespace nn) = "." <> mconcat (fmap (\t -> toText t <> ".") nn)

instance Show Namespace where
    show = unpack . toText

namespaceFromStrings :: [String] -> Maybe Namespace
namespaceFromStrings ss = do
    ns <-
        return $
        fmap fromString $
        case ss of
            "":ns -> ns
            ns -> ns
    for_ ns $ \n ->
        if nameIsUpper n
            then return ()
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

data FullName =
    MkFullName Namespace
               Name
    deriving (Eq, Ord)

pattern RootFullName :: Name -> FullName

pattern RootFullName n = MkFullName RootNamespace n

fullNameToRoot :: FullName -> Maybe Name
fullNameToRoot (RootFullName n) = Just n
fullNameToRoot _ = Nothing

instance ToText FullName where
    toText (MkFullName nsn name) = toText nsn <> toText name

instance Show FullName where
    show = unpack . toText

instance IsString FullName where
    fromString s =
        fromMaybe (error $ "bad FullName: " <> s) $
        fmap RootFullName (infixNameFromString s) <|> do
            nt <- nonEmpty $ splitSeq "." s
            nspace <- namespaceFromStrings $ init nt
            name <- nameFromString $ last nt
            return $ MkFullName nspace name

data NamespaceRef
    = FullNamespaceRef Namespace
    | RelativeNamespaceRef [Name]
    deriving (Eq, Ord)

pattern CurrentNamespaceRef :: NamespaceRef

pattern CurrentNamespaceRef = RelativeNamespaceRef []

pattern RootNamespaceRef :: NamespaceRef

pattern RootNamespaceRef = FullNamespaceRef RootNamespace

class NamespaceRelative t where
    namespaceRelative :: Namespace -> t -> t

instance NamespaceRelative Namespace where
    namespaceRelative n (MkNamespace a) = namespaceConcat n a

instance NamespaceRelative FullName where
    namespaceRelative asn (MkFullName an nn) = MkFullName (namespaceRelative asn an) nn

instance NamespaceRelative t => NamespaceRelative [t] where
    namespaceRelative nsn = fmap $ namespaceRelative nsn

instance ToText NamespaceRef where
    toText (RelativeNamespaceRef nn) = mconcat (fmap (\t -> toText t <> ".") nn)
    toText (FullNamespaceRef asn) = toText asn

instance Show NamespaceRef where
    show = unpack . toText

namespaceRefFromStrings :: [String] -> Maybe NamespaceRef
namespaceRefFromStrings ss =
    case fmap fromString ss of
        "":ns -> do
            for_ ns $ \n ->
                if nameIsUpper n
                    then return ()
                    else Nothing
            return $ FullNamespaceRef $ MkNamespace ns
        ns -> do
            for_ ns $ \n ->
                if nameIsUpper n
                    then return ()
                    else Nothing
            return $ RelativeNamespaceRef ns

instance IsString NamespaceRef where
    fromString s = fromMaybe (error $ "bad NamespaceRef: " <> s) $ namespaceRefFromStrings $ splitSeq "." s

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
    toText (MkFullNameRef ns n) = toText ns <> toText n

instance Show FullNameRef where
    show = unpack . toText

newtype ModuleName =
    MkModuleName (NonEmpty Name)
    deriving (Eq, Ord)

toModuleName :: Text -> Maybe ModuleName
toModuleName t = fmap (MkModuleName . fmap MkName) $ nonEmpty $ splitSeq "." t

instance ToText ModuleName where
    toText (MkModuleName nn) = intercalate "." $ fmap toText $ toList nn

instance Show ModuleName where
    show = unpack . toText

builtInModuleName :: ModuleName
builtInModuleName = MkModuleName $ opoint "Std"
