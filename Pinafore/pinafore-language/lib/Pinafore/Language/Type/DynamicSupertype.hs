module Pinafore.Language.Type.DynamicSupertype where

import Import

type DolanExprShim (ground :: GroundTypeKind) = ComposeShim (DolanOpenExpression ground) (DolanShim ground)

type PolyGreatestDynamicSupertype :: GroundTypeKind -> forall (dv :: CCRVariances) -> CCRVariancesKind dv -> Type
data PolyGreatestDynamicSupertype ground dv gt where
    NullPolyGreatestDynamicSupertype ::
        forall (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv).
        PolyGreatestDynamicSupertype ground dv gt
    MkPolyGreatestDynamicSupertype ::
        forall (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv).
        ( forall (t :: Type).
          CCRPolarArguments dv (DolanType ground) gt 'Negative t -> PShimWit (DolanExprShim ground) (DolanGroundedType ground) 'Negative (Maybe t)
        ) ->
        PolyGreatestDynamicSupertype ground dv gt

nullPolyGreatestDynamicSupertype ::
    forall (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv).
    PolyGreatestDynamicSupertype ground dv gt
nullPolyGreatestDynamicSupertype = NullPolyGreatestDynamicSupertype

simplePolyGreatestDynamicSupertype ::
    forall (ground :: GroundTypeKind) (pt :: Type) (t :: Type).
    ground '[] pt ->
    DolanShim ground pt (Maybe t) ->
    PolyGreatestDynamicSupertype ground '[] t
simplePolyGreatestDynamicSupertype wt conv =
    MkPolyGreatestDynamicSupertype $ \NilCCRArguments ->
        MkShimWit (MkDolanGroundedType wt NilCCRArguments) $ MkPolarShim $ pureComposeShim conv
