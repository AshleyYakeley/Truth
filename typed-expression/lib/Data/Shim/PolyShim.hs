module Data.Shim.PolyShim where

import Data.Shim.PolyMap
import Data.Shim.Variance
import Shapes

type AllInCategory :: PolyShimKind -> Constraint
type AllInCategory pshim = forall k. CoercibleKind k => InCategory (pshim k)

class AllInCategory pshim => ApplyPolyShim (pshim :: PolyShimKind) where
    applyPolyShim ::
           forall k (v :: Variance) (f :: VarianceKind v -> k) (g :: VarianceKind v -> k) (a :: VarianceKind v) (b :: VarianceKind v).
           (InKind a, InKind b, HasVariance v f, HasVariance v g)
        => VarianceType v
        -> pshim (VarianceKind v -> k) f g
        -> VarianceCategory (pshim Type) v a b
        -> pshim k (f a) (g b)
