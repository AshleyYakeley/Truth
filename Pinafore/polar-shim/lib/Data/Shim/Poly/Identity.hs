module Data.Shim.Poly.Identity where

import Shapes

import Data.Shim.Mono
import Data.Shim.Poly.Shim

type IdentityPolyShim :: PolyShimKind
data IdentityPolyShim k a b where
    MkIdentityPolyShim :: forall k (a :: k). IdentityPolyShim k a a

instance Category (IdentityPolyShim k) where
    id = MkIdentityPolyShim
    MkIdentityPolyShim . MkIdentityPolyShim = MkIdentityPolyShim

instance Groupoid (IdentityPolyShim k) where
    invert MkIdentityPolyShim = MkIdentityPolyShim

instance CartesianShim (IdentityPolyShim Type) where
    funcShim MkIdentityPolyShim MkIdentityPolyShim = MkIdentityPolyShim
    pairShim MkIdentityPolyShim MkIdentityPolyShim = MkIdentityPolyShim
    eitherShim MkIdentityPolyShim MkIdentityPolyShim = MkIdentityPolyShim

instance ReduciblePolyShim IdentityPolyShim where
    type ReducedPolyShim IdentityPolyShim = IdentityPolyShim
