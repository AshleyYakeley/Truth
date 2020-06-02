module Language.Expression.Dolan.Simplify.DuplicateGroundTypes
    ( mergeDuplicateGroundTypes
    ) where

import Data.Shim
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

mergeInSingularType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity t
    -> PShimWit (DolanPolyShim ground Type) (DolanSingularType ground) polarity t
mergeInSingularType (GroundDolanSingularType gt args) =
    case mapDolanArguments mergeDuplicateGroundTypesInType (groundTypeVarianceType gt) (groundTypeVarianceMap gt) args of
        MkShimWit args' conv -> MkShimWit (GroundDolanSingularType gt args') conv
mergeInSingularType t = mkShimWit t

mergeInTypes ::
       forall (ground :: GroundTypeKind) polarity ta tb. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity ta
    -> DolanType ground polarity tb
    -> PShimWit (DolanPolyShim ground Type) (DolanType ground) polarity (JoinMeetType polarity ta tb)
mergeInTypes ta tb =
    case polarityType @polarity of
        PositiveType ->
            chainShimWit mergeDuplicateGroundTypesInType $ joinMeetDolanShimWit (mkShimWit ta) (mkShimWit tb)
        NegativeType ->
            chainShimWit mergeDuplicateGroundTypesInType $ joinMeetDolanShimWit (mkShimWit ta) (mkShimWit tb)

mergeIn1SingularType ::
       forall (ground :: GroundTypeKind) polarity t1 tr. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity t1
    -> DolanType ground polarity tr
    -> PShimWit (DolanPolyShim ground Type) (DolanType ground) polarity (JoinMeetType polarity t1 tr)
mergeIn1SingularType ts NilDolanType = mkShimWit $ ConsDolanType ts NilDolanType
mergeIn1SingularType (GroundDolanSingularType gt1 args1) (ConsDolanType (GroundDolanSingularType gt2 args2) tr)
    | Just (Refl, HRefl) <- groundTypeTestEquality gt1 gt2 =
        case mergeDolanArguments mergeInTypes (groundTypeVarianceType gt1) (groundTypeVarianceMap gt1) args1 args2 of
            MkShimWit args' convargs ->
                ccontramap (polarBimap convargs id . polarSwapRight) $
                mergeIn1SingularType (GroundDolanSingularType gt1 args') tr
mergeIn1SingularType ts (ConsDolanType t1 tr) =
    case mergeIn1SingularType ts tr of
        MkShimWit tsr conv ->
            MkShimWit (ConsDolanType t1 tsr) $ polarF (polar2 . conv . polar1) (polarBimap id $ conv . polar2)

mergeDuplicateGroundTypesInType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> PShimWit (DolanPolyShim ground Type) (DolanType ground) polarity t
mergeDuplicateGroundTypesInType NilDolanType = mkShimWit NilDolanType
mergeDuplicateGroundTypesInType (ConsDolanType t1 tr) =
    case mergeInSingularType t1 of
        MkShimWit t1' conv1 ->
            case mergeDuplicateGroundTypesInType tr of
                MkShimWit tr' convr -> ccontramap (polarBimap conv1 convr) $ mergeIn1SingularType t1' tr'

mergeDuplicateGroundTypes ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanGroundType ground, PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) a)
    => a
    -> a
mergeDuplicateGroundTypes =
    mapPShimWits @_ @(DolanType ground) mergeDuplicateGroundTypesInType mergeDuplicateGroundTypesInType
