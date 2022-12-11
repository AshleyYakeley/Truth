module Pinafore.Language.Name.NamedText where

import Pinafore.Language.Name.FullName
import Pinafore.Language.Name.FullNameRef
import Pinafore.Language.Name.Name
import Pinafore.Language.Name.NamespaceRef
import Pinafore.Language.Name.ToText
import Shapes

newtype NamedText =
    MkNamedText ((FullName -> Text) -> Text)
    deriving (Semigroup, Monoid)

runNamedText :: (FullName -> Text) -> NamedText -> Text
runNamedText ft (MkNamedText ftt) = ftt ft

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
    toNamedText fn = MkNamedText $ \fnt -> fnt fn

instance ToNamedText FullNameRef where
    toNamedText (MkFullNameRef (AbsoluteNamespaceRef ns) n) = toNamedText $ MkFullName ns n
    toNamedText r = toNamedText $ toText r

instance IsString NamedText where
    fromString s = toNamedText (pack s :: Text)

instance ToText NamedText where
    toText = runNamedText $ toText . fullNameRootRelative
