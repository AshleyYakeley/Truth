module Pinafore.Language.Type.DynamicSupertype where

import Data.Shim
import Language.Expression.Dolan
import Shapes

makeNilGDS ::
       forall (ground :: GroundTypeKind) (t :: Type) (dt :: Type). IsDolanGroundType ground
    => ground '[] dt
    -> DolanPolyShim ground Type dt (Maybe t)
    -> DolanSingularShimWit ground 'Negative (Maybe t)
makeNilGDS wt conv = mapShimWit (MkPolarMap conv) $ mkShimWit $ GroundedDolanSingularType wt NilCCRArguments

type PolyGreatestDynamicSupertype :: GroundTypeKind -> forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
type PolyGreatestDynamicSupertype ground dv gt
     = forall (t :: Type).
               DolanArguments dv (DolanType ground) gt 'Negative t -> Maybe (DolanSingularShimWit ground 'Negative (Maybe t))
