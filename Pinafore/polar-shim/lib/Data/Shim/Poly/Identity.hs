module Data.Shim.Poly.Identity where

import Data.Shim.Mono
import Data.Shim.Poly.Shim
import Shapes

type IdentityShim :: PolyShimKind
data IdentityShim k a b where
    MkIdentityShim :: forall k (a :: k). IdentityShim k a a

instance Category (IdentityShim k) where
    id = MkIdentityShim
    MkIdentityShim . MkIdentityShim = MkIdentityShim

instance Groupoid (IdentityShim k) where
    invert MkIdentityShim = MkIdentityShim

instance CartesianShim (IdentityShim Type) where
    funcShim MkIdentityShim MkIdentityShim = MkIdentityShim
    pairShim MkIdentityShim MkIdentityShim = MkIdentityShim
    eitherShim MkIdentityShim MkIdentityShim = MkIdentityShim

instance ReduciblePolyShim IdentityShim where
    type ReducedPolyShim IdentityShim = IdentityShim
