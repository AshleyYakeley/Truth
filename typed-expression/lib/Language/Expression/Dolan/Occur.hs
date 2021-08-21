module Language.Expression.Dolan.Occur
    ( occursInType
    , occursInSingularType
    ) where

import Data.Shim
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Shapes

occursInArg ::
       forall (ground :: GroundTypeKind) polarity n sv a. IsDolanGroundType ground
    => CCRVarianceType sv
    -> SymbolType n
    -> SingleArgument sv (DolanType ground) polarity a
    -> Bool
occursInArg CoCCRVarianceType n t = occursInType n t
occursInArg ContraCCRVarianceType n t = occursInType n t
occursInArg RangeCCRVarianceType n (MkRangeType tp tq) = occursInType n tp || occursInType n tq

occursInArgs ::
       forall (ground :: GroundTypeKind) polarity n dv t a. IsDolanGroundType ground
    => DolanVarianceType dv
    -> SymbolType n
    -> DolanArguments dv (DolanType ground) t polarity a
    -> Bool
occursInArgs NilListType _ NilDolanArguments = False
occursInArgs (ConsListType svt dvt) n (ConsDolanArguments arg args) =
    occursInArg @ground @polarity svt n arg || occursInArgs dvt n args

occursInSingularType ::
       forall (ground :: GroundTypeKind) polarity name a. IsDolanGroundType ground
    => SymbolType name
    -> DolanSingularType ground polarity a
    -> Bool
occursInSingularType n (VarDolanSingularType nt)
    | Just Refl <- testEquality n nt = True
occursInSingularType _ (VarDolanSingularType _) = False
occursInSingularType n (GroundedDolanSingularType gt args) = occursInArgs (groundTypeVarianceType gt) n args
occursInSingularType n (RecursiveDolanSingularType n' _)
    | Just Refl <- testEquality n n' = False
occursInSingularType n (RecursiveDolanSingularType _ pt) = occursInType n pt

occursInType ::
       forall (ground :: GroundTypeKind) polarity name a. IsDolanGroundType ground
    => SymbolType name
    -> DolanType ground polarity a
    -> Bool
occursInType _ NilDolanType = False
occursInType n (ConsDolanType t1 t2) = occursInSingularType n t1 || occursInType n t2
