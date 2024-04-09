module Pinafore.Language.Type.DynamicSupertype where

import Data.Shim
import Language.Expression.Dolan
import Shapes

type PolyGreatestDynamicSupertype :: GroundTypeKind -> forall (dv :: CCRVariances) -> CCRVariancesKind dv -> Type
data PolyGreatestDynamicSupertype ground dv gt where
    MkPolyGreatestDynamicSupertype
        :: forall (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv).
           (forall (t :: Type).
                    CCRPolarArguments dv (DolanType ground) gt 'Negative t -> DolanM ground (Maybe (DolanGroundedShimWit ground 'Negative (Maybe t))))
        -> PolyGreatestDynamicSupertype ground dv gt

nullPolyGreatestDynamicSupertype ::
       forall (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv). IsDolanGroundType ground
    => PolyGreatestDynamicSupertype ground dv gt
nullPolyGreatestDynamicSupertype = MkPolyGreatestDynamicSupertype $ \_ -> return Nothing

getPolyGreatestDynamicSupertype ::
       forall (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv) (t :: Type).
       IsDolanGroundType ground
    => PolyGreatestDynamicSupertype ground dv gt
    -> CCRPolarArguments dv (DolanType ground) gt 'Negative t
    -> DolanM ground (Maybe (DolanGroundedShimWit ground 'Negative (Maybe t)))
getPolyGreatestDynamicSupertype (MkPolyGreatestDynamicSupertype f) args = f args

simpleMPolyGreatestDynamicSupertype ::
       forall (ground :: GroundTypeKind) (dt :: Type) (gt :: Type). IsDolanGroundType ground
    => ground '[] dt
    -> DolanM ground (DolanShim ground dt (Maybe gt))
    -> PolyGreatestDynamicSupertype ground '[] gt
simpleMPolyGreatestDynamicSupertype wt mconv =
    MkPolyGreatestDynamicSupertype $ \NilCCRArguments -> do
        conv <- mconv
        return $ Just $ MkShimWit (MkDolanGroundedType wt NilCCRArguments) (MkPolarShim conv)

simplePolyGreatestDynamicSupertype ::
       forall (ground :: GroundTypeKind) (dt :: Type) (gt :: Type). IsDolanGroundType ground
    => ground '[] dt
    -> DolanShim ground dt (Maybe gt)
    -> PolyGreatestDynamicSupertype ground '[] gt
simplePolyGreatestDynamicSupertype wt conv = simpleMPolyGreatestDynamicSupertype wt $ return conv
