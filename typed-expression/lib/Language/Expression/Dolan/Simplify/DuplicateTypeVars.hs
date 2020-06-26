module Language.Expression.Dolan.Simplify.DuplicateTypeVars
    ( mergeDuplicateTypeVars
    ) where

import Data.Shim
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeVariable
import Shapes

mergeInSingularType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity t
    -> DolanSingularShimWit ground polarity t
mergeInSingularType = mapDolanSingularType mergeDuplicateTypeVarsInType

mergeIn1SingularType ::
       forall (ground :: GroundTypeKind) polarity t1 tr. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity t1
    -> DolanPlainType ground polarity tr
    -> DolanPlainShimWit ground polarity (JoinMeetType polarity t1 tr)
mergeIn1SingularType ts NilDolanPlainType = mkShimWit $ ConsDolanPlainType ts NilDolanPlainType
mergeIn1SingularType (VarDolanSingularType vn1) (ConsDolanPlainType (VarDolanSingularType vn2) tr)
    | Just Refl <- testEquality vn1 vn2 =
        ccontramap (polarF polar1 id :: DolanPolarMap ground polarity _ _) $
        mergeIn1SingularType (VarDolanSingularType vn1) tr
mergeIn1SingularType ts (ConsDolanPlainType t1 tr) =
    case mergeIn1SingularType ts tr of
        MkShimWit tsr conv ->
            MkShimWit (ConsDolanPlainType t1 tsr) $ polarF (polar2 . conv . polar1) (iPolarPair id $ conv . polar2)

mergeDuplicateTypeVarsInPlainType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanPlainType ground polarity t
    -> DolanPlainShimWit ground polarity t
mergeDuplicateTypeVarsInPlainType NilDolanPlainType = mkShimWit NilDolanPlainType
mergeDuplicateTypeVarsInPlainType (ConsDolanPlainType t1 tr) =
    case mergeInSingularType t1 of
        MkShimWit t1' conv1 ->
            case mergeDuplicateTypeVarsInPlainType tr of
                MkShimWit tr' convr -> ccontramap (iPolarPair conv1 convr) $ mergeIn1SingularType t1' tr'

mergeDuplicateTypeVarsInType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> DolanShimWit ground polarity t
mergeDuplicateTypeVarsInType (PlainDolanType t) =
    chainShimWit (mkShimWit . PlainDolanType) $ mergeDuplicateTypeVarsInPlainType t
mergeDuplicateTypeVarsInType (RecursiveDolanType n t) =
    plainRecursiveDolanShimWit (uVarName n) $ mergeDuplicateTypeVarsInPlainType t

mergeDuplicateTypeVars ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanGroundType ground, PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) a)
    => a
    -> a
mergeDuplicateTypeVars = mapPShimWits @_ @(DolanType ground) mergeDuplicateTypeVarsInType mergeDuplicateTypeVarsInType
