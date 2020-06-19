module Language.Expression.Dolan.TypeSystem where

import Data.Shim
import Language.Expression.Dolan.Variance
import Shapes

type GroundTypeKind = forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type

type DolanTypeSystem :: GroundTypeKind -> Type
data DolanTypeSystem ground

type DolanPolyShim :: GroundTypeKind -> PolyShimKind
type family DolanPolyShim ground

type DolanPolyIsoShim :: GroundTypeKind -> PolyShimKind
type DolanPolyIsoShim ground = PolyIso (DolanPolyShim ground)

type DolanPolarMap :: GroundTypeKind -> Polarity -> ShimKind Type
type DolanPolarMap ground = PolarMap (DolanPolyShim ground Type)

type DolanName :: GroundTypeKind -> Type
type family DolanName ground
