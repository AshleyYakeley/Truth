module Language.Expression.Dolan.TypeSystem where

import Data.Shim
import Shapes

type GroundTypeKind = forall (dv :: CCRVariances) -> CCRVariancesKind dv -> Type

type DolanTypeSystem :: GroundTypeKind -> Type
data DolanTypeSystem ground

type DolanPolyShim :: GroundTypeKind -> PolyShimKind
type family DolanPolyShim ground

type DolanShim :: GroundTypeKind -> ShimKind Type
type DolanShim ground = DolanPolyShim ground Type

type DolanPolyIsoShim :: GroundTypeKind -> PolyShimKind
type DolanPolyIsoShim ground = PolyIso (DolanPolyShim ground)

type DolanIsoShim :: GroundTypeKind -> ShimKind Type
type DolanIsoShim ground = DolanPolyIsoShim ground Type

type DolanPolarShim :: GroundTypeKind -> Polarity -> ShimKind Type
type DolanPolarShim ground = PolarShim (DolanShim ground)
