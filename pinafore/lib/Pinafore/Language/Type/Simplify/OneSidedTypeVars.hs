module Pinafore.Language.Type.Simplify.OneSidedTypeVars
    ( eliminateOneSidedTypeVars
    ) where

import Language.Expression.Dolan
import Pinafore.Language.Type.Bisubstitute
import Pinafore.Language.Type.Simplify.VarUses
import Pinafore.Language.Type.Type
import Shapes

getEliminateBisubs ::
       forall baseedit t. (TypeMappable (PinaforeType baseedit) t)
    => t
    -> [PinaforeBisubstitutionM Identity baseedit]
getEliminateBisubs expr = let
    (setFromList -> posvars, setFromList -> negvars) = mappableGetVars @baseedit expr
    posonlyvars :: FiniteSet _
    posonlyvars = difference posvars negvars
    negonlyvars :: FiniteSet _
    negonlyvars = difference negvars posvars
    mkbisub :: AnyW SymbolType -> PinaforeBisubstitutionM Identity baseedit
    mkbisub (MkAnyW vn) =
        MkBisubstitution
            vn
            (return $ contramap (\_ -> error "bad bisubstitution") $ mkTypeF NilPinaforeType)
            (return $ fmap (\_ -> error "bad bisubstitution") $ mkTypeF NilPinaforeType)
    in toList $ fmap mkbisub $ posonlyvars <> negonlyvars

eliminateOneSidedTypeVars ::
       forall baseedit a. TypeMappable (PinaforeType baseedit) a
    => a
    -> a
eliminateOneSidedTypeVars expr = runIdentity $ bisubstitutes (getEliminateBisubs expr) expr
