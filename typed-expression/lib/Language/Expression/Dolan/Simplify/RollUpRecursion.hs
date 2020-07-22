module Language.Expression.Dolan.Simplify.RollUpRecursion
    ( rollUpRecursiveTypes
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.MapType
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Recursive
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unroll
import Shapes

type RollUp :: GroundTypeKind -> Type
data RollUp ground where
    MkRollUp
        :: forall (ground :: GroundTypeKind) (polarity :: Polarity) t. Is PolarityType polarity
        => DolanType ground polarity t
        -> DolanIsoShimWit ground polarity t
        -> RollUp ground

mkRollUp ::
       forall (ground :: GroundTypeKind) (polarity :: Polarity) name t.
       (IsDolanGroundType ground, Is PolarityType polarity)
    => SymbolType name
    -> DolanType ground polarity t
    -> Maybe (RollUp ground)
mkRollUp var rolled = do
    MkShimWit unrolled conv <- return $ unrollRecursiveType var rolled
    assignUVar @Type @t var $
        return $ MkRollUp unrolled $ mapShimWitT (isoPolarPoly recursiveIso . invert conv) $ mkShimWitT rolled

rollUpThisType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => [RollUp ground]
    -> DolanType ground polarity t
    -> Writer Any (DolanIsoShimWit ground polarity t)
rollUpThisType [] pt = return $ mkShimWit pt
rollUpThisType (MkRollUp rpt (rt :: _ polarity' _):rr) pt = do
    t' <-
        fromMaybe (return $ mkShimWit pt) $ do
            Refl <- eitherLeft $ samePolarity @polarity @polarity'
            Refl <- testEquality pt rpt
            return $ do
                tell $ Any True
                return rt
    chainShimWitM (rollUpThisType rr) t'

rollUpChildrenType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => [RollUp ground]
    -> DolanType ground polarity t
    -> Writer Any (DolanIsoShimWit ground polarity t)
rollUpChildrenType _rr NilDolanType = return $ nilDolanShimWit
rollUpChildrenType rr (ConsDolanType t1 tr) = do
    t1' <- mapDolanSingularTypeM (rollUpAllType rr) t1
    tr' <- rollUpChildrenType rr tr
    return $ joinMeetShimWit (singleDolanShimWit t1') tr'

rollUpAllType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => [RollUp ground]
    -> DolanType ground polarity t
    -> Writer Any (DolanIsoShimWit ground polarity t)
rollUpAllType rr t = do
    t' <- rollUpThisType rr t
    chainShimWitM (rollUpChildrenType rr) t'

keepRolling ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => [RollUp ground]
    -> DolanType ground polarity t
    -> DolanIsoShimWit ground polarity t
keepRolling [] t = mkShimWit t
keepRolling rr t = let
    (t', Any x) = runWriter $ rollUpAllType rr t
    in if x
           then chainShimWit (keepRolling rr) t'
           else t'

getRollUpsInSingularType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity t
    -> [RollUp ground]
getRollUpsInSingularType (VarDolanSingularType _) = []
getRollUpsInSingularType (GroundDolanSingularType gt args) =
    forDolanArguments getRollUpsInType (groundTypeVarianceType gt) args
getRollUpsInSingularType (RecursiveDolanSingularType var t) =
    case mkRollUp var t of
        Just rollup -> rollup : getRollUpsInType t
        Nothing -> getRollUpsInType t

getRollUpsInType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> [RollUp ground]
getRollUpsInType NilDolanType = []
getRollUpsInType (ConsDolanType t1 tr) = getRollUpsInSingularType t1 <> getRollUpsInType tr

rollUpInType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> DolanIsoShimWit ground polarity t
rollUpInType t = keepRolling (getRollUpsInType t) t

rollUpRecursiveTypes ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanGroundType ground, PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) a)
    => a
    -> a
rollUpRecursiveTypes =
    mapPShimWits
        @_
        @(DolanType ground)
        (reshimWit polyIsoForwards . rollUpInType)
        (reshimWit polyIsoForwards . rollUpInType)
