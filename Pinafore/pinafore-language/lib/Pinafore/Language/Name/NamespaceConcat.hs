module Pinafore.Language.Name.NamespaceConcat where

import Pinafore.Language.Name.FullNameRef
import Pinafore.Language.Name.NamespaceRef
import Shapes

class NamespaceConcat t where
    namespaceConcat :: NamespaceRef -> t -> t

instance NamespaceConcat NamespaceRef where
    namespaceConcat (AbsoluteNamespaceRef ns1) ns2 = AbsoluteNamespaceRef $ namespaceConcatRef ns1 ns2
    namespaceConcat _ (AbsoluteNamespaceRef ns) = AbsoluteNamespaceRef ns
    namespaceConcat (RelativeNamespaceRef ns1) (RelativeNamespaceRef ns2) = RelativeNamespaceRef $ ns1 <> ns2

instance NamespaceConcat FullNameRef where
    namespaceConcat ns (MkFullNameRef nns nn) = MkFullNameRef (namespaceConcat ns nns) nn

instance {-# OVERLAPPABLE #-} (Functor f, NamespaceConcat t) => NamespaceConcat (f t) where
    namespaceConcat nsn = fmap $ namespaceConcat nsn
