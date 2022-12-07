module Pinafore.Language.Name.NamespaceRelative where

import Pinafore.Language.Name.FullNameRef
import Pinafore.Language.Name.NamespaceRef
import Shapes

class NamespaceRelative t where
    namespaceRelative :: NamespaceRef -> t -> t

instance NamespaceRelative NamespaceRef where
    namespaceRelative (AbsoluteNamespaceRef ns1) ns2 = AbsoluteNamespaceRef $ namespaceRefInNamespace ns1 ns2
    namespaceRelative _ (AbsoluteNamespaceRef ns) = AbsoluteNamespaceRef ns
    namespaceRelative (RelativeNamespaceRef ns1) (RelativeNamespaceRef ns2) = RelativeNamespaceRef $ ns1 <> ns2

instance NamespaceRelative FullNameRef where
    namespaceRelative ns (MkFullNameRef nns nn) = MkFullNameRef (namespaceRelative ns nns) nn

instance {-# OVERLAPPABLE #-} (Functor f, NamespaceRelative t) => NamespaceRelative (f t) where
    namespaceRelative nsn = fmap $ namespaceRelative nsn
