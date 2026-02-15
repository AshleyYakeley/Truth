module Pinafore.Syntax.Name.Show.NamedText where

import Shapes

import Pinafore.Syntax.Name.FullName
import Pinafore.Syntax.Name.FullNameRef
import Pinafore.Syntax.Name.NamedText
import Pinafore.Syntax.Name.Namespace
import Pinafore.Syntax.Name.NamespaceRef
import Pinafore.Syntax.Name.Show.Text

class ShowNamedText t where
    showNamedText :: t -> NamedText
    default showNamedText :: ShowText t => t -> NamedText
    showNamedText = toNamedText . showText

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
