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

{-
class TypeFreeVariables (t :: Type) where
    typeFreeVariables :: t -> FiniteSet (AnyW SymbolType)

instance forall (ground :: GroundTypeKind) polarity t. IsDolanGroundType ground =>
             TypeFreeVariables (DolanType ground polarity t) where
    typeFreeVariables NilDolanType = mempty
    typeFreeVariables (ConsDolanType t1 tr) = union (typeFreeVariables t1) (typeFreeVariables tr)

instance forall (ground :: GroundTypeKind) polarity t. IsDolanGroundType ground =>
             TypeFreeVariables (DolanSingularType ground polarity t) where
    typeFreeVariables (GroundDolanSingularType gt args) = argumentsFreeVariables (groundTypeVarianceType gt) args
    typeFreeVariables (VarDolanSingularType v) = singletonSet $ MkAnyW v
    typeFreeVariables (RecursiveDolanSingularType v t) = deleteSet (MkAnyW v) $ typeFreeVariables t

argumentFreeVariables ::
       forall (ground :: GroundTypeKind) polarity v t. IsDolanGroundType ground
    => VarianceType v
    -> SingleArgument v (DolanType ground) polarity t
    -> FiniteSet (AnyW SymbolType)
argumentFreeVariables CovarianceType t = typeFreeVariables t
argumentFreeVariables ContravarianceType t = typeFreeVariables t
argumentFreeVariables RangevarianceType (MkRangeType p q) = union (typeFreeVariables p) (typeFreeVariables q)

argumentsFreeVariables ::
       forall (ground :: GroundTypeKind) polarity dv t ta. IsDolanGroundType ground
    => DolanVarianceType dv
    -> DolanArguments dv (DolanType ground) t polarity ta
    -> FiniteSet (AnyW SymbolType)
argumentsFreeVariables NilListType NilDolanArguments = mempty
argumentsFreeVariables (ConsListType sv dv) (ConsDolanArguments t1 tr) =
    union (argumentFreeVariables @ground @polarity sv t1) (argumentsFreeVariables dv tr)
-}
occursInArg ::
       forall (ground :: GroundTypeKind) polarity n sv a. IsDolanGroundType ground
    => VarianceType sv
    -> SymbolType n
    -> SingleArgument sv (DolanType ground) polarity a
    -> Bool
occursInArg CovarianceType n t = occursInType n t
occursInArg ContravarianceType n t = occursInType n t
occursInArg RangevarianceType n (MkRangeType tp tq) = occursInType n tp || occursInType n tq

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
occursInSingularType n (GroundDolanSingularType gt args) = occursInArgs (groundTypeVarianceType gt) n args
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
