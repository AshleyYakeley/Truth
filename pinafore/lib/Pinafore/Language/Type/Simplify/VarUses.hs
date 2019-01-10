module Pinafore.Language.Type.Simplify.VarUses
    ( mappableGetVarUses
    , mappableGetVars
    ) where

import Language.Expression.Dolan
import Pinafore.Language.GroundType
import Pinafore.Language.Type.Type
import Shapes

class GetVarUses t where
    -- | (positive, negative)
    getVarUses :: t -> ([[AnyW SymbolWitness]], [[AnyW SymbolWitness]])

instance IsTypePolarity polarity => GetVarUses (RangeType (PinaforeType baseedit) polarity a) where
    getVarUses (MkRangeType tp tq) = invertPolarity @polarity $ getVarUses tp <> getVarUses tq

getArgExpressionVarUses ::
       forall baseedit polarity sv a. IsTypePolarity polarity
    => SingleVarianceType sv
    -> SingleArgument sv (PinaforeType baseedit) polarity a
    -> ([[AnyW SymbolWitness]], [[AnyW SymbolWitness]])
getArgExpressionVarUses CovarianceType t = getVarUses t
getArgExpressionVarUses ContravarianceType t = invertPolarity @polarity $ getVarUses t
getArgExpressionVarUses RangevarianceType t = getVarUses t

getArgsExpressionVarUses ::
       forall baseedit polarity dv gt t. IsTypePolarity polarity
    => DolanVarianceType dv
    -> DolanArguments dv (PinaforeType baseedit) gt polarity t
    -> ([[AnyW SymbolWitness]], [[AnyW SymbolWitness]])
getArgsExpressionVarUses NilListType NilDolanArguments = mempty
getArgsExpressionVarUses (ConsListType sv dv) (ConsDolanArguments arg args) =
    getArgExpressionVarUses @baseedit @polarity sv arg <> getArgsExpressionVarUses dv args

instance IsTypePolarity polarity => GetVarUses (PinaforeSingularType baseedit polarity t) where
    getVarUses (GroundPinaforeSingularType gt args) = getArgsExpressionVarUses (pinaforeGroundTypeKind gt) args
    getVarUses (VarPinaforeSingularType _) = mempty

getVarUses' ::
       IsTypePolarity polarity => PinaforeType baseedit polarity t -> ([[AnyW SymbolWitness]], [[AnyW SymbolWitness]])
getVarUses' NilPinaforeType = mempty
getVarUses' (ConsPinaforeType t1 tr) = getVarUses t1 <> getVarUses' tr

getJMSingleTypeVars :: IsTypePolarity polarity => PinaforeSingularType baseedit polarity t -> [AnyW SymbolWitness]
getJMSingleTypeVars (VarPinaforeSingularType vn) = [MkAnyW vn]
getJMSingleTypeVars (GroundPinaforeSingularType _ _) = []

getJMTypeVars :: IsTypePolarity polarity => PinaforeType baseedit polarity t -> [AnyW SymbolWitness]
getJMTypeVars NilPinaforeType = mempty
getJMTypeVars (ConsPinaforeType t1 tr) = getJMSingleTypeVars t1 <> getJMTypeVars tr

instance IsTypePolarity polarity => GetVarUses (PinaforeType baseedit polarity t) where
    getVarUses t =
        case getJMTypeVars t of
            tv ->
                (case whichTypePolarity @polarity of
                     Left Refl -> ([tv], [])
                     Right Refl -> ([], [tv])) <>
                getVarUses' t

mappableGetVarUses ::
       forall baseedit a. TypeMappable (PinaforeType baseedit) a
    => a
    -> ([[AnyW SymbolWitness]], [[AnyW SymbolWitness]])
mappableGetVarUses a =
    mconcat $
    fmap
        (\case
             Left (MkAnyW t) -> getVarUses t
             Right (MkAnyW t) -> getVarUses t) $
    mappableGetTypes @_ @(PinaforeType baseedit) a

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

mappableGetVars ::
       forall baseedit a. TypeMappable (PinaforeType baseedit) a
    => a
    -> ([AnyW SymbolWitness], [AnyW SymbolWitness])
mappableGetVars a =
    mconcat $
    fmap
        (\case
             Left (MkAnyW t) -> getExpressionVars t
             Right (MkAnyW t) -> getExpressionVars t) $
    mappableGetTypes @_ @(PinaforeType baseedit) a
