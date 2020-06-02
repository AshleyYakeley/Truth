module Data.Shim.PolyShim where

import Data.Shim.PolyMap
import Data.Shim.Variance
import Shapes

type AllInCategory :: PolyMapKind -> Constraint
type AllInCategory pmap = forall k. CoercibleKind k => InCategory (pmap k)

class AllInCategory pmap => ApplyPolyShim (pmap :: PolyMapKind) where
    applyPolyShim ::
           forall k (v :: Variance) (f :: VarianceKind v -> k) (g :: VarianceKind v -> k) (a :: VarianceKind v) (b :: VarianceKind v).
           (InKind a, InKind b, HasVariance v f, HasVariance v g)
        => VarianceType v
        -> pmap (VarianceKind v -> k) f g
        -> VarianceCategory (pmap Type) v a b
        -> pmap k (f a) (g b)
