module Pinafore.Language.Type.DynamicSupertype where

import Import

type DolanGroundedMShimWit :: GroundTypeKind -> Polarity -> Type -> Type
type DolanGroundedMShimWit ground polarity
     = PShimWit (ComposeShim (DolanM ground) (DolanShim ground)) (DolanGroundedType ground) polarity

type PolyGreatestDynamicSupertype :: GroundTypeKind -> forall (dv :: CCRVariances) -> CCRVariancesKind dv -> Type
data PolyGreatestDynamicSupertype ground dv gt where
    NullPolyGreatestDynamicSupertype
        :: forall (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv).
           PolyGreatestDynamicSupertype ground dv gt
    MkPolyGreatestDynamicSupertype
        :: forall (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv).
           (forall (t :: Type).
                    CCRPolarArguments dv (DolanType ground) gt 'Negative t -> DolanGroundedMShimWit ground 'Negative (Maybe t))
        -> PolyGreatestDynamicSupertype ground dv gt

nullPolyGreatestDynamicSupertype ::
       forall (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv). IsDolanGroundType ground
    => PolyGreatestDynamicSupertype ground dv gt
nullPolyGreatestDynamicSupertype = NullPolyGreatestDynamicSupertype

getPolyGreatestDynamicSupertype ::
       forall (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv) (t :: Type).
       IsDolanGroundType ground
    => PolyGreatestDynamicSupertype ground dv gt
    -> CCRPolarArguments dv (DolanType ground) gt 'Negative t
    -> DolanM ground (Maybe (DolanGroundedShimWit ground 'Negative (Maybe t)))
getPolyGreatestDynamicSupertype NullPolyGreatestDynamicSupertype _ = return Nothing
getPolyGreatestDynamicSupertype (MkPolyGreatestDynamicSupertype f) args =
    case f args of
        MkShimWit t (MkPolarShim (MkComposeShim mshim)) -> do
            shim <- mshim
            return $ Just $ MkShimWit t $ MkPolarShim shim

simpleMPolyGreatestDynamicSupertype ::
       forall (ground :: GroundTypeKind) (dt :: Type) (gt :: Type). IsDolanGroundType ground
    => ground '[] dt
    -> DolanM ground (DolanShim ground dt (Maybe gt))
    -> PolyGreatestDynamicSupertype ground '[] gt
simpleMPolyGreatestDynamicSupertype wt mconv =
    MkPolyGreatestDynamicSupertype $ \NilCCRArguments ->
        MkShimWit (MkDolanGroundedType wt NilCCRArguments) $ MkPolarShim $ MkComposeShim mconv

simplePolyGreatestDynamicSupertype ::
       forall (ground :: GroundTypeKind) (dt :: Type) (gt :: Type). IsDolanGroundType ground
    => ground '[] dt
    -> DolanShim ground dt (Maybe gt)
    -> PolyGreatestDynamicSupertype ground '[] gt
simplePolyGreatestDynamicSupertype wt conv = simpleMPolyGreatestDynamicSupertype wt $ return conv
