module Language.Expression.Dolan.TypeSystem where

import Data.Shim
import Language.Expression.Dolan.Variance
import Shapes

type GroundTypeKind = forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type

type DolanTypeSystem :: GroundTypeKind -> Type
data DolanTypeSystem ground

type DolanPolyShim :: GroundTypeKind -> PolyMapKind
type family DolanPolyShim ground

type DolanPolarMap :: GroundTypeKind -> Polarity -> MapKind Type
type DolanPolarMap ground = PolarMap (DolanPolyShim ground Type)

type DolanName :: GroundTypeKind -> Type
type family DolanName ground
