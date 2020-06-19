module Language.Expression.Dolan.Simplify.RollUpRecursion
    ( rollUpRecursiveTypes
    ) where

import Data.Shim
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

type RollUp :: GroundTypeKind -> Type
data RollUp ground where
    MkRollUp
        :: forall (ground :: GroundTypeKind) (polarity :: Polarity) t. Is PolarityType polarity
        => DolanPlainType ground polarity t
        -> DolanShimWit ground polarity t
        -> RollUp ground

mkRollUp ::
       forall (ground :: GroundTypeKind) (polarity :: Polarity) t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> RollUp ground
mkRollUp rolled =
    case dolanTypeToPlainUnroll rolled of
        MkShimWit unrolled conv -> MkRollUp unrolled $ MkShimWit rolled $ polarPolyIsoBackwards conv

rollUpThisPlainType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => [RollUp ground]
    -> DolanPlainType ground polarity t
    -> Writer Any (DolanShimWit ground polarity t)
rollUpThisPlainType [] pt = return $ plainDolanShimWit $ mkShimWit pt
rollUpThisPlainType (MkRollUp rpt (rt :: _ polarity' _):rr) pt = do
    t' <-
        fromMaybe (return $ plainDolanShimWit $ mkShimWit pt) $ do
            Refl <- eitherLeft $ samePolarity @polarity @polarity'
            Refl <- testEquality pt rpt
            return $ do
                tell $ Any True
                return rt
    chainShimWitM (rollUpThisType rr) t'

rollUpThisType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => [RollUp ground]
    -> DolanType ground polarity t
    -> Writer Any (DolanShimWit ground polarity t)
rollUpThisType rr (PlainDolanType pt) = rollUpThisPlainType rr pt
rollUpThisType rr (RecursiveDolanType n pt) = do
    t' <- rollUpThisPlainType rr pt
    return $ recursiveDolanShimWit n t'

rollUpChildrenPlainType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => [RollUp ground]
    -> DolanPlainType ground polarity t
    -> Writer Any (DolanShimWit ground polarity t)
rollUpChildrenPlainType _rr NilDolanPlainType = return $ plainDolanShimWit nilDolanPlainShimWit
rollUpChildrenPlainType rr (ConsDolanPlainType t1 tr) = do
    t1' <- mapDolanSingularTypeM (rollUpAllType rr) t1
    tr' <- rollUpChildrenPlainType rr tr
    return $ joinMeetShimWit (singleDolanShimWit t1') tr'

rollUpChildrenType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => [RollUp ground]
    -> DolanType ground polarity t
    -> Writer Any (DolanShimWit ground polarity t)
rollUpChildrenType rr (PlainDolanType pt) = rollUpChildrenPlainType rr pt
rollUpChildrenType rr (RecursiveDolanType n pt) = do
    t' <- rollUpChildrenPlainType rr pt
    return $ recursiveDolanShimWit n t'

rollUpAllType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => [RollUp ground]
    -> DolanType ground polarity t
    -> Writer Any (DolanShimWit ground polarity t)
rollUpAllType rr t = do
    t' <- rollUpThisType rr t
    chainShimWitM (rollUpChildrenType rr) t'

keepRolling ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => [RollUp ground]
    -> DolanType ground polarity t
    -> DolanShimWit ground polarity t
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

getRollUpsInPlainType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanPlainType ground polarity t
    -> [RollUp ground]
getRollUpsInPlainType NilDolanPlainType = []
getRollUpsInPlainType (ConsDolanPlainType t1 tr) = getRollUpsInSingularType t1 <> getRollUpsInPlainType tr

getRollUpsInType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> [RollUp ground]
getRollUpsInType (PlainDolanType pt) = getRollUpsInPlainType pt
getRollUpsInType t@(RecursiveDolanType _ pt) = mkRollUp t : getRollUpsInPlainType pt

rollUpInType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> DolanShimWit ground polarity t
rollUpInType t = keepRolling (getRollUpsInType t) t

rollUpRecursiveTypes ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanGroundType ground, PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) a)
    => a
    -> a
rollUpRecursiveTypes = mapPShimWits @_ @(DolanType ground) rollUpInType rollUpInType
