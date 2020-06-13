module Language.Expression.Dolan.Simplify.DuplicateGroundTypes
    ( mergeDuplicateGroundTypes
    ) where

import Data.Shim
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

mergeInSingularType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity t
    -> DolanSingularShimWit ground polarity t
mergeInSingularType = mapDolanSingularType mergeDuplicateGroundTypesInType

mergeInTypes ::
       forall (ground :: GroundTypeKind) polarity ta tb. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity ta
    -> DolanType ground polarity tb
    -> DolanShimWit ground polarity (JoinMeetType polarity ta tb)
mergeInTypes ta tb =
    case polarityType @polarity of
        PositiveType ->
            chainShimWit mergeDuplicateGroundTypesInType $ joinMeetDolanShimWit (mkShimWit ta) (mkShimWit tb)
        NegativeType ->
            chainShimWit mergeDuplicateGroundTypesInType $ joinMeetDolanShimWit (mkShimWit ta) (mkShimWit tb)

mergeIn1SingularType ::
       forall (ground :: GroundTypeKind) polarity t1 tr. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity t1
    -> DolanPlainType ground polarity tr
    -> DolanPlainShimWit ground polarity (JoinMeetType polarity t1 tr)
mergeIn1SingularType ts NilDolanPlainType = mkShimWit $ ConsDolanPlainType ts NilDolanPlainType
mergeIn1SingularType (GroundDolanSingularType gt1 args1) (ConsDolanPlainType (GroundDolanSingularType gt2 args2) tr)
    | Just (Refl, HRefl) <- groundTypeTestEquality gt1 gt2 =
        case mergeDolanArguments mergeInTypes (groundTypeVarianceType gt1) (groundTypeVarianceMap gt1) args1 args2 of
            MkShimWit args' convargs ->
                ccontramap (polarBimap convargs id . polarSwapRight) $
                mergeIn1SingularType (GroundDolanSingularType gt1 args') tr
mergeIn1SingularType ts (ConsDolanPlainType t1 tr) =
    case mergeIn1SingularType ts tr of
        MkShimWit tsr conv ->
            MkShimWit (ConsDolanPlainType t1 tsr) $ polarF (polar2 . conv . polar1) (polarBimap id $ conv . polar2)

mergeDuplicateGroundTypesInPlainType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanPlainType ground polarity t
    -> DolanPlainShimWit ground polarity t
mergeDuplicateGroundTypesInPlainType NilDolanPlainType = mkShimWit NilDolanPlainType
mergeDuplicateGroundTypesInPlainType (ConsDolanPlainType t1 tr) =
    case mergeInSingularType t1 of
        MkShimWit t1' conv1 ->
            case mergeDuplicateGroundTypesInPlainType tr of
                MkShimWit tr' convr -> ccontramap (polarBimap conv1 convr) $ mergeIn1SingularType t1' tr'

mergeDuplicateGroundTypesInType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> DolanShimWit ground polarity t
mergeDuplicateGroundTypesInType (PlainDolanType pt) =
    case mergeDuplicateGroundTypesInPlainType pt of
        MkShimWit pt' conv -> MkShimWit (PlainDolanType pt') conv
mergeDuplicateGroundTypesInType (RecursiveDolanType n pt) =
    plainRecursiveDolanShimWit n $ mergeDuplicateGroundTypesInPlainType pt

mergeDuplicateGroundTypes ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanGroundType ground, PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) a)
    => a
    -> a
mergeDuplicateGroundTypes =
    mapPShimWits @_ @(DolanType ground) mergeDuplicateGroundTypesInType mergeDuplicateGroundTypesInType
