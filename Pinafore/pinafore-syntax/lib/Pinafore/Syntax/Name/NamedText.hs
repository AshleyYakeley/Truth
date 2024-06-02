module Pinafore.Syntax.Name.NamedText where

import Pinafore.Base
import Pinafore.Syntax.Name.FullName
import Pinafore.Syntax.Name.FullNameRef
import Pinafore.Syntax.Name.Namespace
import Pinafore.Syntax.Name.NamespaceRef
import Pinafore.Syntax.Text
import Shapes

data NamedTextItem
    = FullNameNTI FullName
    | NamespaceNTI Namespace

instance ToText NamedTextItem where
    toText (FullNameNTI fn) = showText fn
    toText (NamespaceNTI ns) = showText ns

newtype NamedText =
    MkNamedText ((NamedTextItem -> Text) -> Text)
    deriving (Semigroup, Monoid)

instance Eq NamedText where
    a == b = toText a == toText b

mapNamedText :: (Text -> Text) -> NamedText -> NamedText
mapNamedText m (MkNamedText ntt) = MkNamedText $ \nt -> m $ ntt nt

apNamedText :: (Text -> Text -> Text) -> NamedText -> NamedText -> NamedText
apNamedText f (MkNamedText ntta) (MkNamedText nttb) = MkNamedText $ \nt -> f (ntta nt) (nttb nt)

bindNamedText :: NamedText -> (Text -> NamedText) -> NamedText
bindNamedText (MkNamedText ntt) f =
    MkNamedText $ \nt ->
        case f $ ntt nt of
            MkNamedText ntt' -> ntt' nt

runNamedText :: (NamedTextItem -> Text) -> NamedText -> Text
runNamedText ft (MkNamedText ftt) = ftt ft

instance ToText NamedText where
    toText = runNamedText toText

runRelativeNamedText :: [Namespace] -> NamedText -> Text
runRelativeNamedText basenss =
    runNamedText $ \case
        FullNameNTI fn -> showText $ relativeFullName basenss fn
        NamespaceNTI ns -> showText $ relativeNamespace basenss ns

-- | for text-like types only
class ToNamedText t where
    toNamedText :: t -> NamedText
    default toNamedText :: ToText t => t -> NamedText
    toNamedText x = MkNamedText $ \_ -> toText x

instance ToNamedText NamedText where
    toNamedText t = t

instance ToNamedText Text

instance ToNamedText String

instance IsString NamedText where
    fromString s = toNamedText (pack s :: Text)

class ShowNamedText t where
    showNamedText :: t -> NamedText
    default showNamedText :: ShowText t => t -> NamedText
    showNamedText x = MkNamedText $ \_ -> showText x

instance {-# OVERLAPPABLE #-} ShowText t => ShowNamedText t

instance ShowNamedText FullName where
    showNamedText fn = MkNamedText $ \fnt -> fnt $ FullNameNTI fn

instance ShowNamedText FullNameRef where
    showNamedText (MkFullNameRef n (AbsoluteNamespaceRef ns)) = showNamedText $ MkFullName n ns
    showNamedText r = toNamedText $ showText r

instance ShowNamedText Namespace where
    showNamedText ns = MkNamedText $ \fnt -> fnt $ NamespaceNTI ns

instance ShowNamedText NamespaceRef where
    showNamedText (AbsoluteNamespaceRef ns) = showNamedText ns
    showNamedText r = toNamedText $ showText r
