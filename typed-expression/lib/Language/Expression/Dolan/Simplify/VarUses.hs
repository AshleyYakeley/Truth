module Language.Expression.Dolan.Simplify.VarUses
    ( mappableGetVarUses
    , mappableGetVars
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Shapes

class GetVarUses f where
    -- | (positive, negative)
    getVarUses :: forall t. f t -> ([[AnyW SymbolType]], [[AnyW SymbolType]])

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             GetVarUses (RangeType (DolanType ground) polarity) where
    getVarUses (MkRangeType tp tq) = invertPolarity @polarity $ getVarUses tp <> getVarUses tq

instance GetVarUses wit => GetVarUses (ShimWit cat wit polarity) where
    getVarUses (MkShimWit w _) = getVarUses w

getArgExpressionVarUses ::
       forall (ground :: GroundTypeKind) polarity sv a. (IsDolanGroundType ground, Is PolarityType polarity)
    => VarianceType sv
    -> SingleArgument sv (DolanType ground) polarity a
    -> ([[AnyW SymbolType]], [[AnyW SymbolType]])
getArgExpressionVarUses CovarianceType t = getVarUses t
getArgExpressionVarUses ContravarianceType t = invertPolarity @polarity $ getVarUses t
getArgExpressionVarUses RangevarianceType t = getVarUses t

getArgsExpressionVarUses ::
       forall (ground :: GroundTypeKind) polarity dv gt t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanVarianceType dv
    -> DolanArguments dv (DolanType ground) gt polarity t
    -> ([[AnyW SymbolType]], [[AnyW SymbolType]])
getArgsExpressionVarUses NilListType NilDolanArguments = mempty
getArgsExpressionVarUses (ConsListType sv dv) (ConsDolanArguments arg args) =
    getArgExpressionVarUses @ground @polarity sv arg <> getArgsExpressionVarUses dv args

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             GetVarUses (DolanSingularType ground polarity) where
    getVarUses (GroundDolanSingularType gt args) = getArgsExpressionVarUses (groundTypeVarianceType gt) args
    getVarUses (VarDolanSingularType _) = mempty

getVarUses' ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => (DolanType ground) polarity t
    -> ([[AnyW SymbolType]], [[AnyW SymbolType]])
getVarUses' NilDolanType = mempty
getVarUses' (ConsDolanType t1 tr) = getVarUses t1 <> getVarUses' tr

getJMSingleTypeVars ::
       forall (ground :: GroundTypeKind) polarity t. Is PolarityType polarity
    => DolanSingularType ground polarity t
    -> [AnyW SymbolType]
getJMSingleTypeVars (VarDolanSingularType vn) = [MkAnyW vn]
getJMSingleTypeVars (GroundDolanSingularType _ _) = []

getJMTypeVars ::
       forall (ground :: GroundTypeKind) polarity t. Is PolarityType polarity
    => (DolanType ground) polarity t
    -> [AnyW SymbolType]
getJMTypeVars NilDolanType = mempty
getJMTypeVars (ConsDolanType t1 tr) = getJMSingleTypeVars @ground t1 <> getJMTypeVars @ground tr

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             GetVarUses (DolanType ground polarity) where
    getVarUses t =
        case getJMTypeVars t of
            tv ->
                (case polarityType @polarity of
                     PositiveType -> ([tv], [])
                     NegativeType -> ([], [tv])) <>
                getVarUses' @ground t

mappableGetVarUses ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) a =>
               a -> ([[AnyW SymbolType]], [[AnyW SymbolType]])
mappableGetVarUses a =
    mconcat $
    fmap
        (\case
             Left (MkAnyW t) -> getVarUses t
             Right (MkAnyW t) -> getVarUses t) $
    mappableGetWitnesses
        @_
        @(PShimWit (DolanPolyShim ground Type) (DolanType ground) 'Positive)
        @(PShimWit (DolanPolyShim ground Type) (DolanType ground) 'Negative)
        a

class GetExpressionVars f where
    -- | (positive, negative)
    getExpressionVars :: forall t. f t -> ([AnyW SymbolType], [AnyW SymbolType])

instance GetExpressionVars wit => GetExpressionVars (ShimWit cat wit polarity) where
    getExpressionVars (MkShimWit w _) = getExpressionVars w

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             GetExpressionVars (RangeType (DolanType ground) polarity) where
    getExpressionVars (MkRangeType tp tq) = invertPolarity @polarity $ getExpressionVars tp <> getExpressionVars tq

getArgExpressionVars ::
       forall (ground :: GroundTypeKind) polarity sv a. (IsDolanGroundType ground, Is PolarityType polarity)
    => VarianceType sv
    -> SingleArgument sv (DolanType ground) polarity a
    -> ([AnyW SymbolType], [AnyW SymbolType])
getArgExpressionVars CovarianceType t = getExpressionVars t
getArgExpressionVars ContravarianceType t = invertPolarity @polarity $ getExpressionVars t
getArgExpressionVars RangevarianceType t = getExpressionVars t

getArgsExpressionVars ::
       forall (ground :: GroundTypeKind) polarity dv gt t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanVarianceType dv
    -> DolanArguments dv (DolanType ground) gt polarity t
    -> ([AnyW SymbolType], [AnyW SymbolType])
getArgsExpressionVars NilListType NilDolanArguments = mempty
getArgsExpressionVars (ConsListType sv dv) (ConsDolanArguments arg args) =
    getArgExpressionVars @ground @polarity sv arg <> getArgsExpressionVars dv args

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             GetExpressionVars (DolanSingularType ground polarity) where
    getExpressionVars (GroundDolanSingularType gt args) = getArgsExpressionVars (groundTypeVarianceType gt) args
    getExpressionVars (VarDolanSingularType vn) =
        case polarityType @polarity of
            PositiveType -> ([MkAnyW vn], [])
            NegativeType -> ([], [MkAnyW vn])

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             GetExpressionVars ((DolanType ground) polarity) where
    getExpressionVars NilDolanType = mempty
    getExpressionVars (ConsDolanType t1 tr) = getExpressionVars t1 <> getExpressionVars tr

mappableGetVars ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) a => a -> ([AnyW SymbolType], [AnyW SymbolType])
mappableGetVars a =
    mconcat $
    fmap
        (\case
             Left (MkAnyW t) -> getExpressionVars t
             Right (MkAnyW t) -> getExpressionVars t) $
    mappableGetWitnesses
        @_
        @(PShimWit (DolanPolyShim ground Type) (DolanType ground) 'Positive)
        @(PShimWit (DolanPolyShim ground Type) (DolanType ground) 'Negative)
        a
