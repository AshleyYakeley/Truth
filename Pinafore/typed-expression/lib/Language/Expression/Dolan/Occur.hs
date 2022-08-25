module Language.Expression.Dolan.Occur
    ( occursInType
    , occursInSingularType
    ) where

import Language.Expression.Dolan.Argument
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

occursInArg ::
       forall (ground :: GroundTypeKind) polarity n sv a. IsDolanGroundType ground
    => SymbolType n
    -> CCRPolarArgument (DolanType ground) polarity sv a
    -> Bool
occursInArg n (CoCCRPolarArgument t) = occursInType n t
occursInArg n (ContraCCRPolarArgument t) = occursInType n t
occursInArg n (RangeCCRPolarArgument tp tq) = occursInType n tp || occursInType n tq

occursInArgs ::
       forall (ground :: GroundTypeKind) polarity n dv t a. IsDolanGroundType ground
    => SymbolType n
    -> DolanArguments dv (DolanType ground) t polarity a
    -> Bool
occursInArgs _ NilCCRArguments = False
occursInArgs n (ConsCCRArguments arg args) = occursInArg @ground @polarity n arg || occursInArgs n args

occursInSingularType ::
       forall (ground :: GroundTypeKind) polarity name a. IsDolanGroundType ground
    => SymbolType name
    -> DolanSingularType ground polarity a
    -> Bool
occursInSingularType n (VarDolanSingularType nt)
    | Just Refl <- testEquality n nt = True
occursInSingularType _ (VarDolanSingularType _) = False
occursInSingularType n (GroundedDolanSingularType (MkDolanGroundedType _ args)) = occursInArgs n args
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
