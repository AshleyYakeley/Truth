module Language.Expression.Dolan.Simplify.UnusedRecursion
    ( eliminateUnusedRecursion
    ) where

import Data.Shim
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Shapes

elimInArg ::
       forall (ground :: GroundTypeKind) polarity v t. (IsDolanGroundType ground, Is PolarityType polarity)
    => VarianceType v
    -> SingleArgument v (DolanType ground) polarity t
    -> SingleArgument v (DolanType ground) polarity t
elimInArg CovarianceType t = elimInType t
elimInArg ContravarianceType t = invertPolarity @polarity $ elimInType t
elimInArg RangevarianceType (MkRangeType tp tq) = invertPolarity @polarity $ MkRangeType (elimInType tp) (elimInType tq)

elimInArgs ::
       forall (ground :: GroundTypeKind) polarity dv t ta. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanVarianceType dv
    -> DolanArguments dv (DolanType ground) t polarity ta
    -> DolanArguments dv (DolanType ground) t polarity ta
elimInArgs NilListType NilDolanArguments = NilDolanArguments
elimInArgs (ConsListType svt dvt) (ConsDolanArguments arg args) =
    ConsDolanArguments (elimInArg @ground @polarity svt arg) (elimInArgs dvt args)

elimInSingularType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity t
    -> DolanSingularType ground polarity t
elimInSingularType t@(VarDolanSingularType _) = t
elimInSingularType (GroundDolanSingularType gt args) =
    GroundDolanSingularType gt $ elimInArgs (groundTypeVarianceType gt) args

elimInPlainType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanPlainType ground polarity t
    -> DolanPlainType ground polarity t
elimInPlainType NilDolanPlainType = NilDolanPlainType
elimInPlainType (ConsDolanPlainType t1 tr) = ConsDolanPlainType (elimInSingularType t1) $ elimInPlainType tr

elimInType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> DolanType ground polarity t
elimInType t
    | Just pt <- dolanTypeToPlainNonrec t = PlainDolanType $ elimInPlainType pt
elimInType (PlainDolanType pt) = PlainDolanType $ elimInPlainType pt
elimInType (RecursiveDolanType n pt) = RecursiveDolanType n $ elimInPlainType pt

elimInTypeWit ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> DolanShimWit ground polarity t
elimInTypeWit t = mkShimWit $ elimInType t

eliminateUnusedRecursion ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanGroundType ground, PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) a)
    => a
    -> a
eliminateUnusedRecursion = mapPShimWits @_ @(DolanType ground) elimInTypeWit elimInTypeWit
