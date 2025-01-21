module Data.Shim.Poly.Neutral where

import Shapes

import Data.Shim.Mono
import Data.Shim.Poly.Shim

type NeutralShim :: PolyShimKind
data NeutralShim k a b where
    IdentityNeutralShim :: forall k (a :: k). NeutralShim k a a
    CoerceNeutralShim ::
        forall k (a :: k) (b :: k).
        Coercible a b =>
        NeutralShim k a b

instance Category (NeutralShim k) where
    id = IdentityNeutralShim
    IdentityNeutralShim . s = s
    s . IdentityNeutralShim = s
    CoerceNeutralShim . CoerceNeutralShim = CoerceNeutralShim

instance Groupoid (NeutralShim k) where
    invert IdentityNeutralShim = IdentityNeutralShim
    invert CoerceNeutralShim = CoerceNeutralShim

neutralShimToCoercion :: NeutralShim k a b -> Coercion a b
neutralShimToCoercion IdentityNeutralShim = MkCoercion
neutralShimToCoercion CoerceNeutralShim = MkCoercion

neutralShimToShim ::
    forall k (shim :: ShimKind k) (a :: k) (b :: k).
    CoerceShim shim =>
    NeutralShim k a b ->
    shim a b
neutralShimToShim IdentityNeutralShim = id
neutralShimToShim CoerceNeutralShim = coercionToShim "neutral" MkCoercion

instance CartesianShim (NeutralShim Type) where
    funcShim IdentityNeutralShim IdentityNeutralShim = IdentityNeutralShim
    funcShim (neutralShimToCoercion -> MkCoercion) (neutralShimToCoercion -> MkCoercion) = CoerceNeutralShim
    pairShim IdentityNeutralShim IdentityNeutralShim = IdentityNeutralShim
    pairShim (neutralShimToCoercion -> MkCoercion) (neutralShimToCoercion -> MkCoercion) = CoerceNeutralShim
    eitherShim IdentityNeutralShim IdentityNeutralShim = IdentityNeutralShim
    eitherShim (neutralShimToCoercion -> MkCoercion) (neutralShimToCoercion -> MkCoercion) = CoerceNeutralShim
