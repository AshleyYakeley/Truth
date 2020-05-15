module Pinafore.Language.TypeSystem.Simplify.VarUses
    ( mappableGetVarUses
    , mappableGetVars
    ) where

import Data.Shim
import Language.Expression.Dolan
import Language.Expression.WitnessMappable
import Pinafore.Language.Type.Ground
import Pinafore.Language.TypeSystem.Type
import Shapes

class GetVarUses f where
    -- | (positive, negative)
    getVarUses :: forall t. f t -> ([[AnyW SymbolType]], [[AnyW SymbolType]])

instance Is PolarityType polarity => GetVarUses (RangeType PinaforeType polarity) where
    getVarUses (MkRangeType tp tq) = invertPolarity @polarity $ getVarUses tp <> getVarUses tq

instance GetVarUses wit => GetVarUses (ShimWit cat wit polarity) where
    getVarUses (MkShimWit w _) = getVarUses w

getArgExpressionVarUses ::
       forall polarity sv a. Is PolarityType polarity
    => VarianceType sv
    -> SingleArgument sv PinaforeType polarity a
    -> ([[AnyW SymbolType]], [[AnyW SymbolType]])
getArgExpressionVarUses CovarianceType t = getVarUses t
getArgExpressionVarUses ContravarianceType t = invertPolarity @polarity $ getVarUses t
getArgExpressionVarUses RangevarianceType t = getVarUses t

getArgsExpressionVarUses ::
       forall polarity dv gt t. Is PolarityType polarity
    => DolanVarianceType dv
    -> DolanArguments dv PinaforeType gt polarity t
    -> ([[AnyW SymbolType]], [[AnyW SymbolType]])
getArgsExpressionVarUses NilListType NilDolanArguments = mempty
getArgsExpressionVarUses (ConsListType sv dv) (ConsDolanArguments arg args) =
    getArgExpressionVarUses @polarity sv arg <> getArgsExpressionVarUses dv args

instance Is PolarityType polarity => GetVarUses (PinaforeSingularType polarity) where
    getVarUses (GroundPinaforeSingularType gt args) = getArgsExpressionVarUses (pinaforeGroundTypeVarianceType gt) args
    getVarUses (VarPinaforeSingularType _) = mempty

getVarUses' :: Is PolarityType polarity => PinaforeType polarity t -> ([[AnyW SymbolType]], [[AnyW SymbolType]])
getVarUses' NilPinaforeType = mempty
getVarUses' (ConsPinaforeType t1 tr) = getVarUses t1 <> getVarUses' tr

getJMSingleTypeVars :: Is PolarityType polarity => PinaforeSingularType polarity t -> [AnyW SymbolType]
getJMSingleTypeVars (VarPinaforeSingularType vn) = [MkAnyW vn]
getJMSingleTypeVars (GroundPinaforeSingularType _ _) = []

getJMTypeVars :: Is PolarityType polarity => PinaforeType polarity t -> [AnyW SymbolType]
getJMTypeVars NilPinaforeType = mempty
getJMTypeVars (ConsPinaforeType t1 tr) = getJMSingleTypeVars t1 <> getJMTypeVars tr

instance Is PolarityType polarity => GetVarUses (PinaforeType polarity) where
    getVarUses t =
        case getJMTypeVars t of
            tv ->
                (case representative @_ @_ @polarity of
                     PositiveType -> ([tv], [])
                     NegativeType -> ([], [tv])) <>
                getVarUses' t

mappableGetVarUses ::
       forall a. PShimWitMappable PinaforeShim PinaforeType a
    => a
    -> ([[AnyW SymbolType]], [[AnyW SymbolType]])
mappableGetVarUses a =
    mconcat $
    fmap
        (\case
             Left (MkAnyW t) -> getVarUses t
             Right (MkAnyW t) -> getVarUses t) $
    mappableGetWitnesses @_ @(PinaforeShimWit 'Positive) @(PinaforeShimWit 'Negative) a

class GetExpressionVars f where
    -- | (positive, negative)
    getExpressionVars :: forall t. f t -> ([AnyW SymbolType], [AnyW SymbolType])

instance GetExpressionVars wit => GetExpressionVars (ShimWit cat wit polarity) where
    getExpressionVars (MkShimWit w _) = getExpressionVars w

instance Is PolarityType polarity => GetExpressionVars (RangeType PinaforeType polarity) where
    getExpressionVars (MkRangeType tp tq) = invertPolarity @polarity $ getExpressionVars tp <> getExpressionVars tq

getArgExpressionVars ::
       forall polarity sv a. Is PolarityType polarity
    => VarianceType sv
    -> SingleArgument sv PinaforeType polarity a
    -> ([AnyW SymbolType], [AnyW SymbolType])
getArgExpressionVars CovarianceType t = getExpressionVars t
getArgExpressionVars ContravarianceType t = invertPolarity @polarity $ getExpressionVars t
getArgExpressionVars RangevarianceType t = getExpressionVars t

getArgsExpressionVars ::
       forall polarity dv gt t. Is PolarityType polarity
    => DolanVarianceType dv
    -> DolanArguments dv PinaforeType gt polarity t
    -> ([AnyW SymbolType], [AnyW SymbolType])
getArgsExpressionVars NilListType NilDolanArguments = mempty
getArgsExpressionVars (ConsListType sv dv) (ConsDolanArguments arg args) =
    getArgExpressionVars @polarity sv arg <> getArgsExpressionVars dv args

instance Is PolarityType polarity => GetExpressionVars (PinaforeSingularType polarity) where
    getExpressionVars (GroundPinaforeSingularType gt args) =
        getArgsExpressionVars (pinaforeGroundTypeVarianceType gt) args
    getExpressionVars (VarPinaforeSingularType vn) =
        case representative @_ @_ @polarity of
            PositiveType -> ([MkAnyW vn], [])
            NegativeType -> ([], [MkAnyW vn])

instance Is PolarityType polarity => GetExpressionVars (PinaforeType polarity) where
    getExpressionVars NilPinaforeType = mempty
    getExpressionVars (ConsPinaforeType t1 tr) = getExpressionVars t1 <> getExpressionVars tr

mappableGetVars ::
       forall a. PShimWitMappable PinaforeShim PinaforeType a
    => a
    -> ([AnyW SymbolType], [AnyW SymbolType])
mappableGetVars a =
    mconcat $
    fmap
        (\case
             Left (MkAnyW t) -> getExpressionVars t
             Right (MkAnyW t) -> getExpressionVars t) $
    mappableGetWitnesses @_ @(PinaforeShimWit 'Positive) @(PinaforeShimWit 'Negative) a
