module Pinafore.Syntax.Name.NamedText where

import Pinafore.Base
import Shapes

import Pinafore.Syntax.Name.FullName
import Pinafore.Syntax.Name.FullNameRef
import Pinafore.Syntax.Name.Namespace
import Pinafore.Syntax.Name.Show.Text

data NamedTextItem
    = FullNameNTI FullName
    | NamespaceNTI Namespace

instance ToText NamedTextItem where
    toText (FullNameNTI fn) = showText fn
    toText (NamespaceNTI ns) = showText ns

newtype NamedText
    = MkNamedText ((NamedTextItem -> Text) -> Text)
    deriving newtype (Semigroup, Monoid)

instance Eq NamedText where
    a == b = toText a == toText b

instance Ord NamedText where
    compare a b = compare (toText a) (toText b)

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
