module Pinafore.Language.Type.Simplify.VarUses
    ( mappableGetVarUses
    , mappableGetVars
    ) where

import Language.Expression.Dolan
import Language.Expression.Polarity
import Pinafore.Language.GroundType
import Pinafore.Language.Type.Type
import Shapes

class GetVarUses t where
    -- | (positive, negative)
    getVarUses :: t -> ([[AnyW SymbolType]], [[AnyW SymbolType]])

instance Is PolarityType polarity => GetVarUses (RangeType (PinaforeType baseedit) polarity a) where
    getVarUses (MkRangeType tp tq) = invertPolarity @polarity $ getVarUses tp <> getVarUses tq

getArgExpressionVarUses ::
       forall baseedit polarity sv a. Is PolarityType polarity
    => SingleVarianceType sv
    -> SingleArgument sv (PinaforeType baseedit) polarity a
    -> ([[AnyW SymbolType]], [[AnyW SymbolType]])
getArgExpressionVarUses CovarianceType t = getVarUses t
getArgExpressionVarUses ContravarianceType t = invertPolarity @polarity $ getVarUses t
getArgExpressionVarUses RangevarianceType t = getVarUses t

getArgsExpressionVarUses ::
       forall baseedit polarity dv gt t. Is PolarityType polarity
    => DolanVarianceType dv
    -> DolanArguments dv (PinaforeType baseedit) gt polarity t
    -> ([[AnyW SymbolType]], [[AnyW SymbolType]])
getArgsExpressionVarUses NilListType NilDolanArguments = mempty
getArgsExpressionVarUses (ConsListType sv dv) (ConsDolanArguments arg args) =
    getArgExpressionVarUses @baseedit @polarity sv arg <> getArgsExpressionVarUses dv args

instance Is PolarityType polarity => GetVarUses (PinaforeSingularType baseedit polarity t) where
    getVarUses (GroundPinaforeSingularType gt args) = getArgsExpressionVarUses (pinaforeGroundTypeKind gt) args
    getVarUses (VarPinaforeSingularType _) = mempty

getVarUses' ::
       Is PolarityType polarity => PinaforeType baseedit polarity t -> ([[AnyW SymbolType]], [[AnyW SymbolType]])
getVarUses' NilPinaforeType = mempty
getVarUses' (ConsPinaforeType t1 tr) = getVarUses t1 <> getVarUses' tr

getJMSingleTypeVars :: Is PolarityType polarity => PinaforeSingularType baseedit polarity t -> [AnyW SymbolType]
getJMSingleTypeVars (VarPinaforeSingularType vn) = [MkAnyW vn]
getJMSingleTypeVars (GroundPinaforeSingularType _ _) = []

getJMTypeVars :: Is PolarityType polarity => PinaforeType baseedit polarity t -> [AnyW SymbolType]
getJMTypeVars NilPinaforeType = mempty
getJMTypeVars (ConsPinaforeType t1 tr) = getJMSingleTypeVars t1 <> getJMTypeVars tr

instance Is PolarityType polarity => GetVarUses (PinaforeType baseedit polarity t) where
    getVarUses t =
        case getJMTypeVars t of
            tv ->
                (case representative @_ @_ @polarity of
                     PositiveType -> ([tv], [])
                     NegativeType -> ([], [tv])) <>
                getVarUses' t

mappableGetVarUses ::
       forall baseedit a. TypeMappable (PinaforeType baseedit) a
    => a
    -> ([[AnyW SymbolType]], [[AnyW SymbolType]])
mappableGetVarUses a =
    mconcat $
    fmap
        (\case
             Left (MkAnyW t) -> getVarUses t
             Right (MkAnyW t) -> getVarUses t) $
    mappableGetTypes @_ @(PinaforeType baseedit) a

class GetExpressionVars t where
    -- | (positive, negative)
    getExpressionVars :: t -> ([AnyW SymbolType], [AnyW SymbolType])

instance Is PolarityType polarity => GetExpressionVars (RangeType (PinaforeType baseedit) polarity a) where
    getExpressionVars (MkRangeType tp tq) = invertPolarity @polarity $ getExpressionVars tp <> getExpressionVars tq

getArgExpressionVars ::
       forall baseedit polarity sv a. Is PolarityType polarity
    => SingleVarianceType sv
    -> SingleArgument sv (PinaforeType baseedit) polarity a
    -> ([AnyW SymbolType], [AnyW SymbolType])
getArgExpressionVars CovarianceType t = getExpressionVars t
getArgExpressionVars ContravarianceType t = invertPolarity @polarity $ getExpressionVars t
getArgExpressionVars RangevarianceType t = getExpressionVars t

getArgsExpressionVars ::
       forall baseedit polarity dv gt t. Is PolarityType polarity
    => DolanVarianceType dv
    -> DolanArguments dv (PinaforeType baseedit) gt polarity t
    -> ([AnyW SymbolType], [AnyW SymbolType])
getArgsExpressionVars NilListType NilDolanArguments = mempty
getArgsExpressionVars (ConsListType sv dv) (ConsDolanArguments arg args) =
    getArgExpressionVars @baseedit @polarity sv arg <> getArgsExpressionVars dv args

instance Is PolarityType polarity => GetExpressionVars (PinaforeSingularType baseedit polarity t) where
    getExpressionVars (GroundPinaforeSingularType gt args) = getArgsExpressionVars (pinaforeGroundTypeKind gt) args
    getExpressionVars (VarPinaforeSingularType vn) =
        case representative @_ @_ @polarity of
            PositiveType -> ([MkAnyW vn], [])
            NegativeType -> ([], [MkAnyW vn])

instance Is PolarityType polarity => GetExpressionVars (PinaforeType baseedit polarity t) where
    getExpressionVars NilPinaforeType = mempty
    getExpressionVars (ConsPinaforeType t1 tr) = getExpressionVars t1 <> getExpressionVars tr

mappableGetVars ::
       forall baseedit a. TypeMappable (PinaforeType baseedit) a
    => a
    -> ([AnyW SymbolType], [AnyW SymbolType])
mappableGetVars a =
    mconcat $
    fmap
        (\case
             Left (MkAnyW t) -> getExpressionVars t
             Right (MkAnyW t) -> getExpressionVars t) $
    mappableGetTypes @_ @(PinaforeType baseedit) a
