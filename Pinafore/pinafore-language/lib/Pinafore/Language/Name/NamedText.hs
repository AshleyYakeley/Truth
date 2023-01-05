module Pinafore.Language.Name.NamedText where

import Pinafore.Language.Name.FullName
import Pinafore.Language.Name.FullNameRef
import Pinafore.Language.Name.Name
import Pinafore.Language.Name.Namespace
import Pinafore.Language.Name.NamespaceRef
import Pinafore.Language.Name.ToText
import Shapes

data NamedTextItem
    = FullNameNTI FullName
    | NamespaceNTI Namespace

newtype NamedText =
    MkNamedText ((NamedTextItem -> Text) -> Text)
    deriving (Semigroup, Monoid)

runNamedText :: (NamedTextItem -> Text) -> NamedText -> Text
runNamedText ft (MkNamedText ftt) = ftt ft

runRelativeNamedText :: [Namespace] -> NamedText -> Text
runRelativeNamedText basenss =
    runNamedText $ \case
        FullNameNTI fn -> toText $ relativeFullName basenss fn
        NamespaceNTI ns -> toText $ relativeNamespace basenss ns

class ToNamedText t where
    toNamedText :: t -> NamedText
    default toNamedText :: ToText t => t -> NamedText
    toNamedText x = MkNamedText $ \_ -> toText x

instance ToNamedText NamedText where
    toNamedText t = t

instance ToNamedText Text

instance ToNamedText Name

instance ToNamedText (SymbolType name)

instance ToNamedText FullName where
    toNamedText fn = MkNamedText $ \fnt -> fnt $ FullNameNTI fn

instance ToNamedText FullNameRef where
    toNamedText (MkFullNameRef n (AbsoluteNamespaceRef ns)) = toNamedText $ MkFullName n ns
    toNamedText r = toNamedText $ toText r

instance ToNamedText Namespace where
    toNamedText ns = MkNamedText $ \fnt -> fnt $ NamespaceNTI ns

instance ToNamedText NamespaceRef where
    toNamedText (AbsoluteNamespaceRef ns) = toNamedText ns
    toNamedText r = toNamedText $ toText r

instance IsString NamedText where
    fromString s = toNamedText (pack s :: Text)

instance ToText NamedText where
    toText =
        runNamedText $ \case
            FullNameNTI fn -> toText $ fullNameRootRelative fn
            NamespaceNTI ns -> toText $ namespaceRootRelative ns
