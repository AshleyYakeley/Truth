module Pinafore.Language.Type.DynamicSupertype where

import Data.Shim
import Language.Expression.Dolan
import Shapes

type PolyGreatestDynamicSupertype :: GroundTypeKind -> forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
data PolyGreatestDynamicSupertype ground dv gt where
    GeneralPolyGreatestDynamicSupertype
        :: forall (ground :: GroundTypeKind) (dv :: DolanVariance) (gt :: DolanVarianceKind dv).
           (forall (t :: Type).
                    DolanArguments dv (DolanType ground) gt 'Negative t -> Maybe (DolanGroundedShimWit ground 'Negative (Maybe t)))
        -> PolyGreatestDynamicSupertype ground dv gt
    SimplePolyGreatestDynamicSupertype
        :: forall (ground :: GroundTypeKind) (dt :: Type) (gt :: Type).
           ground '[] dt
        -> DolanPolyShim ground Type dt (Maybe gt)
        -> DolanPolyShim ground Type gt dt
        -> PolyGreatestDynamicSupertype ground '[] gt

nullPolyGreatestDynamicSupertype ::
       forall (ground :: GroundTypeKind) (dv :: DolanVariance) (gt :: DolanVarianceKind dv).
       PolyGreatestDynamicSupertype ground dv gt
nullPolyGreatestDynamicSupertype = GeneralPolyGreatestDynamicSupertype $ \_ -> Nothing

getPolyGreatestDynamicSupertype ::
       forall (ground :: GroundTypeKind) (dv :: DolanVariance) (gt :: DolanVarianceKind dv) (t :: Type).
       IsDolanGroundType ground
    => PolyGreatestDynamicSupertype ground dv gt
    -> DolanArguments dv (DolanType ground) gt 'Negative t
    -> Maybe (DolanGroundedShimWit ground 'Negative (Maybe t))
getPolyGreatestDynamicSupertype (GeneralPolyGreatestDynamicSupertype f) args = f args
getPolyGreatestDynamicSupertype (SimplePolyGreatestDynamicSupertype wt conv _) NilCCRArguments =
    Just $ MkShimWit (MkDolanGroundedType wt NilCCRArguments) (MkPolarMap conv)
