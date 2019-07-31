module Pinafore.Language.Type.Simplify.OneSidedTypeVars
    ( eliminateOneSidedTypeVars
    ) where

import Data.Shim
import Language.Expression.Dolan
import Pinafore.Language.Type.Bisubstitute
import Pinafore.Language.Type.Simplify.VarUses
import Pinafore.Language.Type.Type
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
             ccontramap (toEnhanced @_ @JMShim "error" $ \_ -> error "bad bisubstitution") $
             mkPJMShimWit NilPinaforeType)
            (return $
             cfmap (toEnhanced @_ @JMShim "error" $ \_ -> error "bad bisubstitution") $ mkPJMShimWit NilPinaforeType)
    in toList $ fmap mkbisub $ posonlyvars <> negonlyvars

eliminateOneSidedTypeVars ::
       forall baseedit a. PShimWitMappable PinaforeShim (PinaforeType baseedit) a
    => a
    -> a
eliminateOneSidedTypeVars expr = runIdentity $ bisubstitutes @baseedit (getEliminateBisubs expr) expr
