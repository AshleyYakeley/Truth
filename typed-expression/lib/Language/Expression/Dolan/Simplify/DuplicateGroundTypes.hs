module Language.Expression.Dolan.Simplify.DuplicateGroundTypes
    ( mergeDuplicateGroundTypes
    ) where

import Data.Shim
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Solver
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeVariable
import Shapes

mergeInSingularType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity t
    -> DolanTypeCheckM ground (DolanSingularShimWit ground polarity t)
mergeInSingularType = mapDolanSingularTypeM mergeDuplicateGroundTypesInType

mergeInTypes ::
       forall (ground :: GroundTypeKind) polarity ta tb. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity ta
    -> DolanType ground polarity tb
    -> DolanTypeCheckM ground (DolanShimWit ground polarity (JoinMeetType polarity ta tb))
mergeInTypes ta tb =
    case polarityType @polarity of
        PositiveType -> chainShimWitM mergeDuplicateGroundTypesInType $ joinMeetShimWit (mkShimWit ta) (mkShimWit tb)
        NegativeType -> chainShimWitM mergeDuplicateGroundTypesInType $ joinMeetShimWit (mkShimWit ta) (mkShimWit tb)

mergeIn1SingularType ::
       forall (ground :: GroundTypeKind) polarity t1 tr. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity t1
    -> DolanPlainType ground polarity tr
    -> DolanTypeCheckM ground (DolanPlainShimWit ground polarity (JoinMeetType polarity t1 tr))
mergeIn1SingularType ts NilDolanPlainType = return $ mkShimWit $ ConsDolanPlainType ts NilDolanPlainType
mergeIn1SingularType (GroundDolanSingularType gt1 args1) (ConsDolanPlainType (GroundDolanSingularType gt2 args2) tr)
    | Just (Refl, HRefl) <- groundTypeTestEquality gt1 gt2 = do
        MkShimWit args' convargs <-
            mergeDolanArgumentsM mergeInTypes (groundTypeVarianceType gt1) (groundTypeVarianceMap gt1) args1 args2
        t' <- mergeIn1SingularType (GroundDolanSingularType gt1 args') tr
        return $ ccontramap (iPolarPair convargs id . iPolarSwapR) t'
mergeIn1SingularType ts (ConsDolanPlainType t1 tr) = do
    MkShimWit tsr conv <- mergeIn1SingularType ts tr
    return $ MkShimWit (ConsDolanPlainType t1 tsr) $ polarF (polar2 . conv . polar1) (iPolarPair id $ conv . polar2)

mergeDuplicateGroundTypesInPlainType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanPlainType ground polarity t
    -> DolanTypeCheckM ground (DolanPlainShimWit ground polarity t)
mergeDuplicateGroundTypesInPlainType NilDolanPlainType = return $ mkShimWit NilDolanPlainType
mergeDuplicateGroundTypesInPlainType (ConsDolanPlainType t1 tr) = do
    MkShimWit t1' conv1 <- mergeInSingularType t1
    MkShimWit tr' convr <- mergeDuplicateGroundTypesInPlainType tr
    t'' <- mergeIn1SingularType t1' tr'
    return $ ccontramap (iPolarPair conv1 convr) t''

mergeDuplicateGroundTypesInType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> DolanTypeCheckM ground (DolanShimWit ground polarity t)
mergeDuplicateGroundTypesInType (PlainDolanType pt) = do
    MkShimWit pt' conv <- mergeDuplicateGroundTypesInPlainType pt
    return $ MkShimWit (PlainDolanType pt') conv
mergeDuplicateGroundTypesInType (RecursiveDolanType n pt) = do
    t' <- mergeDuplicateGroundTypesInPlainType pt
    return $ plainRecursiveDolanShimWit (uVarName n) t'

mergeDuplicateGroundTypes ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanSubtypeGroundType ground, PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) a)
    => a
    -> DolanTypeCheckM ground a
mergeDuplicateGroundTypes = mapPShimWitsM mergeDuplicateGroundTypesInType mergeDuplicateGroundTypesInType
