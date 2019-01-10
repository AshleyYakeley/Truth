module Pinafore.Language.Type.Simplify.OneSidedTypeVars
    ( eliminateOneSidedTypeVarsInType
    , eliminateOneSidedTypeVars
    ) where

import Language.Expression.Dolan
import Pinafore.Language.Type.Bisubstitute
import Pinafore.Language.Type.Simplify.VarUses
import Pinafore.Language.Type.Type
import Shapes

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

eliminateOneSidedTypeVars ::
       forall baseedit a. (GetExpressionVars a, MapTypes (PinaforeType baseedit) a)
    => a
    -> a
eliminateOneSidedTypeVars expr = runIdentity $ bisubstitutes (getEliminateBisubs expr) expr
