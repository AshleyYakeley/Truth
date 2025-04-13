module Data.Shim.Poly.Neutral where

import Shapes

import Data.Shim.Mono
import Data.Shim.Poly.Shim

type NeutralPolyShim :: PolyShimKind
data NeutralPolyShim k a b where
    IdentityNeutralPolyShim :: forall k (a :: k). NeutralPolyShim k a a
    CoerceNeutralPolyShim ::
        forall k (a :: k) (b :: k).
        Coercible a b =>
        NeutralPolyShim k a b

instance Category (NeutralPolyShim k) where
    id = IdentityNeutralPolyShim
    IdentityNeutralPolyShim . s = s
    s . IdentityNeutralPolyShim = s
    CoerceNeutralPolyShim . CoerceNeutralPolyShim = CoerceNeutralPolyShim

instance Groupoid (NeutralPolyShim k) where
    invert IdentityNeutralPolyShim = IdentityNeutralPolyShim
    invert CoerceNeutralPolyShim = CoerceNeutralPolyShim

neutralShimToCoercion :: NeutralPolyShim k a b -> Coercion a b
neutralShimToCoercion IdentityNeutralPolyShim = MkCoercion
neutralShimToCoercion CoerceNeutralPolyShim = MkCoercion

neutralShimToShim ::
    forall k (shim :: ShimKind k) (a :: k) (b :: k).
    CoerceShim shim =>
    NeutralPolyShim k a b ->
    shim a b
neutralShimToShim IdentityNeutralPolyShim = id
neutralShimToShim CoerceNeutralPolyShim = coercionToShim MkCoercion

instance CartesianShim (NeutralPolyShim Type) where
    funcShim IdentityNeutralPolyShim IdentityNeutralPolyShim = IdentityNeutralPolyShim
    funcShim (neutralShimToCoercion -> MkCoercion) (neutralShimToCoercion -> MkCoercion) = CoerceNeutralPolyShim
    pairShim IdentityNeutralPolyShim IdentityNeutralPolyShim = IdentityNeutralPolyShim
    pairShim (neutralShimToCoercion -> MkCoercion) (neutralShimToCoercion -> MkCoercion) = CoerceNeutralPolyShim
    eitherShim IdentityNeutralPolyShim IdentityNeutralPolyShim = IdentityNeutralPolyShim
    eitherShim (neutralShimToCoercion -> MkCoercion) (neutralShimToCoercion -> MkCoercion) = CoerceNeutralPolyShim
