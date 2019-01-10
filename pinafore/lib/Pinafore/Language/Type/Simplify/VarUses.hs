module Pinafore.Language.Type.Simplify.VarUses
    ( GetExpressionVarUses(..)
    , GetExpressionVars(..)
    ) where

import Language.Expression.Dolan
import Language.Expression.Expression
import Language.Expression.Named
import Language.Expression.Pattern
import Language.Expression.Sealed
import Pinafore.Language.GroundType
import Pinafore.Language.Type.Type
import Shapes

class GetExpressionVarUses t where
    -- | (positive, negative)
    getExpressionVarUses :: t -> ([[AnyW SymbolWitness]], [[AnyW SymbolWitness]])

instance IsTypePolarity polarity => GetExpressionVarUses (RangeType (PinaforeType baseedit) polarity a) where
    getExpressionVarUses (MkRangeType tp tq) =
        invertPolarity @polarity $ getExpressionVarUses tp <> getExpressionVarUses tq

getArgExpressionVarUses ::
       forall baseedit polarity sv a. IsTypePolarity polarity
    => SingleVarianceType sv
    -> SingleArgument sv (PinaforeType baseedit) polarity a
    -> ([[AnyW SymbolWitness]], [[AnyW SymbolWitness]])
getArgExpressionVarUses CovarianceType t = getExpressionVarUses t
getArgExpressionVarUses ContravarianceType t = invertPolarity @polarity $ getExpressionVarUses t
getArgExpressionVarUses RangevarianceType t = getExpressionVarUses t

getArgsExpressionVarUses ::
       forall baseedit polarity dv gt t. IsTypePolarity polarity
    => DolanVarianceType dv
    -> DolanArguments dv (PinaforeType baseedit) gt polarity t
    -> ([[AnyW SymbolWitness]], [[AnyW SymbolWitness]])
getArgsExpressionVarUses NilListType NilDolanArguments = mempty
getArgsExpressionVarUses (ConsListType sv dv) (ConsDolanArguments arg args) =
    getArgExpressionVarUses @baseedit @polarity sv arg <> getArgsExpressionVarUses dv args

instance IsTypePolarity polarity => GetExpressionVarUses (PinaforeSingularType baseedit polarity t) where
    getExpressionVarUses (GroundPinaforeSingularType gt args) =
        getArgsExpressionVarUses (pinaforeGroundTypeKind gt) args
    getExpressionVarUses (VarPinaforeSingularType _) = mempty

getExpressionVarUses' ::
       IsTypePolarity polarity => PinaforeType baseedit polarity t -> ([[AnyW SymbolWitness]], [[AnyW SymbolWitness]])
getExpressionVarUses' NilPinaforeType = mempty
getExpressionVarUses' (ConsPinaforeType t1 tr) = getExpressionVarUses t1 <> getExpressionVarUses' tr

getJMSingleTypeVars :: IsTypePolarity polarity => PinaforeSingularType baseedit polarity t -> [AnyW SymbolWitness]
getJMSingleTypeVars (VarPinaforeSingularType vn) = [MkAnyW vn]
getJMSingleTypeVars (GroundPinaforeSingularType _ _) = []

getJMTypeVars :: IsTypePolarity polarity => PinaforeType baseedit polarity t -> [AnyW SymbolWitness]
getJMTypeVars NilPinaforeType = mempty
getJMTypeVars (ConsPinaforeType t1 tr) = getJMSingleTypeVars t1 <> getJMTypeVars tr

instance IsTypePolarity polarity => GetExpressionVarUses (PinaforeType baseedit polarity t) where
    getExpressionVarUses t =
        case getJMTypeVars t of
            tv ->
                (case whichTypePolarity @polarity of
                     Left Refl -> ([tv], [])
                     Right Refl -> ([], [tv])) <>
                getExpressionVarUses' t

instance GetExpressionVarUses (NamedExpression name (PinaforeType baseedit 'NegativePolarity) t) where
    getExpressionVarUses (ClosedExpression _) = mempty
    getExpressionVarUses (OpenExpression (MkNameWitness _ t) expr) = getExpressionVarUses t <> getExpressionVarUses expr

instance GetExpressionVarUses (NamedPattern name (PinaforeType baseedit 'PositivePolarity) a b) where
    getExpressionVarUses (ClosedPattern _) = mempty
    getExpressionVarUses (OpenPattern (MkNameWitness _ t) expr) = getExpressionVarUses t <> getExpressionVarUses expr

instance GetExpressionVarUses (PinaforeExpression baseedit) where
    getExpressionVarUses (MkSealedExpression twt expr) = getExpressionVarUses twt <> getExpressionVarUses expr

instance GetExpressionVarUses (PinaforePattern baseedit) where
    getExpressionVarUses (MkSealedPattern twt expr) = getExpressionVarUses twt <> getExpressionVarUses expr

class GetExpressionVars t where
    -- | (positive, negative)
    getExpressionVars :: t -> ([AnyW SymbolWitness], [AnyW SymbolWitness])

instance IsTypePolarity polarity => GetExpressionVars (RangeType (PinaforeType baseedit) polarity a) where
    getExpressionVars (MkRangeType tp tq) = invertPolarity @polarity $ getExpressionVars tp <> getExpressionVars tq

getArgExpressionVars ::
       forall baseedit polarity sv a. IsTypePolarity polarity
    => SingleVarianceType sv
    -> SingleArgument sv (PinaforeType baseedit) polarity a
    -> ([AnyW SymbolWitness], [AnyW SymbolWitness])
getArgExpressionVars CovarianceType t = getExpressionVars t
getArgExpressionVars ContravarianceType t = invertPolarity @polarity $ getExpressionVars t
getArgExpressionVars RangevarianceType t = getExpressionVars t

getArgsExpressionVars ::
       forall baseedit polarity dv gt t. IsTypePolarity polarity
    => DolanVarianceType dv
    -> DolanArguments dv (PinaforeType baseedit) gt polarity t
    -> ([AnyW SymbolWitness], [AnyW SymbolWitness])
getArgsExpressionVars NilListType NilDolanArguments = mempty
getArgsExpressionVars (ConsListType sv dv) (ConsDolanArguments arg args) =
    getArgExpressionVars @baseedit @polarity sv arg <> getArgsExpressionVars dv args

instance IsTypePolarity polarity => GetExpressionVars (PinaforeSingularType baseedit polarity t) where
    getExpressionVars (GroundPinaforeSingularType gt args) = getArgsExpressionVars (pinaforeGroundTypeKind gt) args
    getExpressionVars (VarPinaforeSingularType vn) =
        case whichTypePolarity @polarity of
            Left Refl -> ([MkAnyW vn], [])
            Right Refl -> ([], [MkAnyW vn])

instance IsTypePolarity polarity => GetExpressionVars (PinaforeType baseedit polarity t) where
    getExpressionVars NilPinaforeType = mempty
    getExpressionVars (ConsPinaforeType t1 tr) = getExpressionVars t1 <> getExpressionVars tr

instance GetExpressionVars (NamedExpression name (PinaforeType baseedit 'NegativePolarity) t) where
    getExpressionVars (ClosedExpression _) = mempty
    getExpressionVars (OpenExpression (MkNameWitness _ t) expr) = getExpressionVars t <> getExpressionVars expr

instance GetExpressionVars (NamedPattern name (PinaforeType baseedit 'PositivePolarity) a b) where
    getExpressionVars (ClosedPattern _) = mempty
    getExpressionVars (OpenPattern (MkNameWitness _ t) expr) = getExpressionVars t <> getExpressionVars expr

instance GetExpressionVars (PinaforeExpression baseedit) where
    getExpressionVars (MkSealedExpression twt expr) = getExpressionVars twt <> getExpressionVars expr

instance GetExpressionVars (PinaforePattern baseedit) where
    getExpressionVars (MkSealedPattern twt pat) = getExpressionVars twt <> getExpressionVars pat
