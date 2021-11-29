module Pinafore.Language.Type.DynamicSupertype where

import Data.Shim
import Language.Expression.Dolan
import Shapes

type GreatestDynamicSupertype :: GroundTypeKind -> PolyShimKind -> Type -> Type
type GreatestDynamicSupertype ground pshim t = PShimWit (pshim Type) (DolanType ground) 'Negative (Maybe t)

makeNilGDS ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (t :: Type) (dt :: Type).
       (IsDolanGroundType ground, JoinMeetIsoCategory (pshim Type))
    => ground '[] dt
    -> pshim Type dt (Maybe t)
    -> GreatestDynamicSupertype ground pshim t
makeNilGDS wt conv =
    mapShimWit (MkPolarMap conv) $ singleDolanShimWit $ mkShimWit $ GroundedDolanSingularType wt NilDolanArguments

type PolyGreatestDynamicSupertype :: GroundTypeKind -> PolyShimKind -> forall (dv :: DolanVariance) ->
                                                                               DolanVarianceKind dv -> Type
type PolyGreatestDynamicSupertype ground pshim dv gt
     = forall (t :: Type).
               DolanArguments dv (DolanType ground) gt 'Positive t -> Maybe (GreatestDynamicSupertype ground pshim t)
