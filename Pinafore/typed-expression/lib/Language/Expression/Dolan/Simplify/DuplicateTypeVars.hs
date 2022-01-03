module Language.Expression.Dolan.Simplify.DuplicateTypeVars
    ( mergeDuplicateTypeVars
    ) where

import Data.Shim
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

mergeInSingularType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity t
    -> DolanSingularShimWit ground polarity t
mergeInSingularType = mapDolanSingularType mergeDuplicateTypeVarsInType

mergeIn1SingularType ::
       forall (ground :: GroundTypeKind) polarity t1 tr. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity t1
    -> DolanType ground polarity tr
    -> DolanShimWit ground polarity (JoinMeetType polarity t1 tr)
mergeIn1SingularType ts NilDolanType = mkPolarShimWit $ ConsDolanType ts NilDolanType
mergeIn1SingularType (VarDolanSingularType vn1) (ConsDolanType (VarDolanSingularType vn2) tr)
    | Just Refl <- testEquality vn1 vn2 =
        mapPolarShimWit (polarF polar1 id) $ mergeIn1SingularType (VarDolanSingularType vn1) tr
mergeIn1SingularType ts (ConsDolanType t1 tr) =
    case mergeIn1SingularType ts tr of
        MkShimWit tsr conv ->
            MkShimWit (ConsDolanType t1 tsr) $ polarF (polar2 . conv . polar1) (iPolarPair id $ conv . polar2)

mergeDuplicateTypeVarsInType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> DolanShimWit ground polarity t
mergeDuplicateTypeVarsInType NilDolanType = nilDolanShimWit
mergeDuplicateTypeVarsInType (ConsDolanType t1 tr) =
    case mergeInSingularType t1 of
        MkShimWit t1' conv1 ->
            case mergeDuplicateTypeVarsInType tr of
                MkShimWit tr' convr -> ccontramap (iPolarPair conv1 convr) $ mergeIn1SingularType t1' tr'

mergeDuplicateTypeVars ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanGroundType ground, PShimWitMappable (DolanShim ground) (DolanType ground) a)
    => a
    -> a
mergeDuplicateTypeVars = mapPShimWits @_ @(DolanType ground) mergeDuplicateTypeVarsInType mergeDuplicateTypeVarsInType
