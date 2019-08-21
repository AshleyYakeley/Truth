module Pinafore.Language.TypeSystem.Simplify.OneSidedTypeVars
    ( eliminateOneSidedTypeVars
    ) where

import Data.Shim
import Language.Expression.Dolan
import Pinafore.Language.TypeSystem.Bisubstitute
import Pinafore.Language.TypeSystem.Simplify.VarUses
import Pinafore.Language.TypeSystem.Type
import Shapes

getEliminateBisubs ::
       forall baseedit t. (PShimWitMappable PinaforeShim (PinaforeType baseedit) t)
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
            (return $
             ccontramap (toEnhanced @_ @JMShim "eliminated" $ \_ -> error "bad bisubstitution") $
             mkPJMShimWit NilPinaforeType)
            (return $
             cfmap (toEnhanced @_ @JMShim "eliminated" $ \_ -> error "bad bisubstitution") $
             mkPJMShimWit NilPinaforeType)
    in toList $ fmap mkbisub $ posonlyvars <> negonlyvars

eliminateOneSidedTypeVars ::
       forall baseedit a. PShimWitMappable PinaforeShim (PinaforeType baseedit) a
    => a
    -> a
eliminateOneSidedTypeVars expr = runIdentity $ bisubstitutes @baseedit (getEliminateBisubs expr) expr
