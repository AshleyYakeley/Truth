module Language.Expression.Dolan.Simplify.VarUsage
    ( TVarUsage (..)
    , getTVarUsage
    )
where

import Data.Shim
import Shapes

import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeSystem

type TVarUsage :: GroundTypeKind -> Polarity -> Type -> Type -> Type
data TVarUsage ground polarity tv a
    = forall b. MkTVarUsage
        (DolanShimWit ground polarity b)
        (DolanPolarShim ground polarity a (JoinMeetType polarity tv b))

swapC ::
    forall shim polarity a b c.
    (JoinMeetIsoShim shim, Is PolarityType polarity) =>
    PolarShim shim polarity (JoinMeetType polarity a (JoinMeetType polarity b c)) (JoinMeetType polarity (JoinMeetType polarity b a) c)
swapC = iPolarPair iPolarSwap id . iPolarSwapR

swapAB ::
    forall shim polarity a b c.
    (JoinMeetIsoShim shim, Is PolarityType polarity) =>
    PolarShim shim polarity (JoinMeetType polarity a (JoinMeetType polarity b c)) (JoinMeetType polarity b (JoinMeetType polarity a c))
swapAB = iPolarSwapL . swapC

getTVarUsage' ::
    forall (ground :: GroundTypeKind) polarity tv a b.
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    TypeVarT tv ->
    DolanType ground polarity a ->
    DolanType ground polarity b ->
    Maybe (TVarUsage ground polarity tv (JoinMeetType polarity a b))
getTVarUsage' _ _ NilDolanType = Nothing
getTVarUsage' v ta (ConsDolanType (VarDolanSingularType v') tb)
    | Just Refl <- testEquality v v' =
        Just
            $ case getTVarUsage v tb of
                Nothing -> MkTVarUsage (joinMeetType ta tb) $ swapAB
                Just (MkTVarUsage tr conv) ->
                    MkTVarUsage (joinMeetShimWit (mkPolarShimWit ta) tr) $ swapAB . iPolarPair id (polarF polar1 conv)
getTVarUsage' v ta (ConsDolanType ts tb) = do
    MkTVarUsage tu conv <- getTVarUsage' v (ConsDolanType ts ta) tb
    return $ MkTVarUsage tu $ conv . swapC

getTVarUsage ::
    forall (ground :: GroundTypeKind) polarity tv a.
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    TypeVarT tv ->
    DolanType ground polarity a ->
    Maybe (TVarUsage ground polarity tv a)
getTVarUsage v t = do
    MkTVarUsage tu conv <- getTVarUsage' v NilDolanType t
    return $ MkTVarUsage tu $ conv . iPolarR2
