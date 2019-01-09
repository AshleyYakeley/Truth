module Pinafore.Language.Type.Simplify.OneSidedTypeVars
    ( eliminateOneSidedTypeVarsInType
    , eliminateOneSidedTypeVarsInExpression
    , eliminateOneSidedTypeVarsInPattern
    ) where

import Language.Expression.Dolan
import Language.Expression.Expression
import Language.Expression.Named
import Language.Expression.Pattern
import Language.Expression.Sealed
import Pinafore.Language.GroundType
import Pinafore.Language.Type.Bisubstitute
import Pinafore.Language.Type.Type
import Shapes

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

eliminateOneSidedTypeVarsInType ::
       forall baseedit polarity t. IsTypePolarity polarity
    => PinaforeType baseedit polarity t
    -> PinaforeTypeF baseedit polarity t
eliminateOneSidedTypeVarsInType t = let
    (setFromList -> posvars, setFromList -> negvars) = getExpressionVars t
    posonlyvars :: FiniteSet _
    posonlyvars = difference posvars negvars
    negonlyvars :: FiniteSet _
    negonlyvars = difference negvars posvars
    mkbisub :: AnyW SymbolWitness -> PinaforeBisubstitutionM Identity baseedit
    mkbisub (MkAnyW vn) =
        MkBisubstitution
            vn
            (return $ contramap (\_ -> error "bad bisubstitution") $ mkTypeF NilPinaforeType)
            (return $ fmap (\_ -> error "bad bisubstitution") $ mkTypeF NilPinaforeType)
    bisubs = toList $ fmap mkbisub $ posonlyvars <> negonlyvars
    in runIdentity $ bisubstitutesType bisubs t

getEliminateBisubs ::
       forall baseedit t. GetExpressionVars t
    => t
    -> [PinaforeBisubstitutionM Identity baseedit]
getEliminateBisubs expr = let
    (setFromList -> posvars, setFromList -> negvars) = getExpressionVars expr
    posonlyvars :: FiniteSet _
    posonlyvars = difference posvars negvars
    negonlyvars :: FiniteSet _
    negonlyvars = difference negvars posvars
    mkbisub :: AnyW SymbolWitness -> PinaforeBisubstitutionM Identity baseedit
    mkbisub (MkAnyW vn) =
        MkBisubstitution
            vn
            (return $ contramap (\_ -> error "bad bisubstitution") $ mkTypeF NilPinaforeType)
            (return $ fmap (\_ -> error "bad bisubstitution") $ mkTypeF NilPinaforeType)
    in toList $ fmap mkbisub $ posonlyvars <> negonlyvars

eliminateOneSidedTypeVarsInExpression :: forall baseedit. PinaforeExpression baseedit -> PinaforeExpression baseedit
eliminateOneSidedTypeVarsInExpression expr = runIdentity $ bisubstitutesSealedExpression (getEliminateBisubs expr) expr

eliminateOneSidedTypeVarsInPattern :: forall baseedit. PinaforePattern baseedit -> PinaforePattern baseedit
eliminateOneSidedTypeVarsInPattern pat = runIdentity $ bisubstitutesSealedPattern (getEliminateBisubs pat) pat
