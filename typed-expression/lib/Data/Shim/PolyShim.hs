module Data.Shim.PolyShim where

import Data.Shim.PolyMap
import Data.Shim.Variance
import Shapes

class (forall k. CoercibleKind k => InCategory (pmap k)) => ApplyPolyShim (pmap :: PolyMapKind) where
    applyPolyShim ::
           forall k (v :: Variance) (f :: VarianceKind v -> k) (g :: VarianceKind v -> k) (a :: VarianceKind v) (b :: VarianceKind v).
           (InKind a, InKind b, HasVariance v f, HasVariance v g)
        => VarianceType v
        -> pmap (VarianceKind v -> k) f g
        -> VarianceCategory (pmap Type) v a b
        -> pmap k (f a) (g b)
