module Language.Expression.Dolan.Simplify.RollUpRecursion
    ( rollUpRecursiveTypes
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.Occur
import Language.Expression.Dolan.PShimWit
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

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => Show (RollUp ground) where
    show (MkRollUp unrolled (MkShimWit rolled _)) = showDolanType unrolled <> " => " <> showDolanType rolled

mkRollUp ::
       forall (ground :: GroundTypeKind) (polarity :: Polarity) tv. (IsDolanGroundType ground, Is PolarityType polarity)
    => TypeVarT tv
    -> DolanType ground polarity tv
    -> RollUp ground
mkRollUp var rolled =
    case unrollRecursiveType var rolled of
        MkShimWit unrolled conv ->
            MkRollUp unrolled $
            mapPolarShimWitT (invert conv) $ shimWitToDolan $ mkPolarShimWitT $ RecursiveDolanSingularType var rolled

rollUpThisType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => [RollUp ground]
    -> DolanType ground polarity t
    -> Writer Any (DolanIsoShimWit ground polarity t)
rollUpThisType [] pt = return $ mkPolarShimWit pt
rollUpThisType (MkRollUp unrolled (rt :: _ (_ polarity') _):rr) pt = do
    t' <-
        fromMaybe (return $ mkPolarShimWit pt) $ do
            Refl <- eitherLeft $ samePolarity @polarity @polarity'
            Refl <- testEquality pt unrolled
            return $ do
                tell $ Any True
                return rt
    chainPolarShimWitM (rollUpThisType rr) t'

rollUpChildrenType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => [RollUp ground]
    -> DolanType ground polarity t
    -> Writer Any (DolanIsoShimWit ground polarity t)
rollUpChildrenType _rr NilDolanType = return $ nilDolanShimWit
rollUpChildrenType rr (ConsDolanType t1 tr) = do
    t1' <- mapDolanSingularTypeM (rollUpAllType rr) t1
    tr' <- rollUpChildrenType rr tr
    return $ joinMeetShimWit (shimWitToDolan t1') tr'

rollUpAllType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => [RollUp ground]
    -> DolanType ground polarity t
    -> Writer Any (DolanIsoShimWit ground polarity t)
rollUpAllType rr t = do
    t' <- rollUpThisType rr t
    chainPolarShimWitM (rollUpChildrenType rr) t'

keepRolling ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => [RollUp ground]
    -> DolanType ground polarity t
    -> DolanIsoShimWit ground polarity t
keepRolling [] t = mkPolarShimWit t
keepRolling rr t = let
    (t', Any x) = runWriter $ rollUpAllType rr t
    in if x
           then chainPolarShimWit (keepRolling rr) t'
           else t'

getRollUpsInSingularType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity t
    -> [RollUp ground]
getRollUpsInSingularType (VarDolanSingularType _) = []
getRollUpsInSingularType (GroundedDolanSingularType (MkDolanGroundedType _ args)) =
    forDolanArguments getRollUpsInType args
getRollUpsInSingularType (RecursiveDolanSingularType var t) =
    if occursInType var t
        then mkRollUp var t : getRollUpsInType t
        else getRollUpsInType t

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
       (IsDolanGroundType ground, PShimWitMappable (DolanShim ground) (DolanType ground) a)
    => Endo a
rollUpRecursiveTypes =
    mapPShimWits
        @_
        @(DolanType ground)
        (reshimWit polyIsoForwards . rollUpInType)
        (reshimWit polyIsoForwards . rollUpInType)
