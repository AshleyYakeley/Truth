module Data.Shim.PolyShim where

import Data.Shim.Variance
import Shapes

-- type PolyShim (shim :: forall kc. kc -> kc -> Type) = forall k. CoercibleKind k => InCategory (shim :: k -> k -> Type)
class (forall k. CoercibleKind k => InCategory (shim :: k -> k -> Type)) =>
          ApPolyShim (shim :: forall kc. kc -> kc -> Type) where
    apShimFunc ::
           forall (v :: Variance) k (f :: VarianceKind v -> k) (g :: VarianceKind v -> k) (a :: VarianceKind v) (b :: VarianceKind v).
           (InKind a, InKind b, HasVariance v f, HasVariance v g)
        => VarianceType v
        -> shim f g
        -> VarianceCategory shim v a b
        -> shim (f a) (g b)
