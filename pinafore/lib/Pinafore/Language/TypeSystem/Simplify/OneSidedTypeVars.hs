module Pinafore.Language.TypeSystem.Simplify.OneSidedTypeVars
    ( eliminateOneSidedTypeVars
    ) where

import Data.Shim
import Language.Expression.Dolan
import Pinafore.Language.Shim
import Pinafore.Language.TypeSystem.Bisubstitute
import Pinafore.Language.TypeSystem.Simplify.VarUses
import Pinafore.Language.TypeSystem.Type
import Shapes

getEliminateBisubs ::
       forall t. (PShimWitMappable (PinaforeShim Type) PinaforeType t)
    => t
    -> [PinaforeBisubstitutionM Identity]
getEliminateBisubs expr = let
    (setFromList -> posvars, setFromList -> negvars) = mappableGetVars expr
    posonlyvars :: FiniteSet _
    posonlyvars = difference posvars negvars
    negonlyvars :: FiniteSet _
    negonlyvars = difference negvars posvars
    mkbisub :: AnyW SymbolType -> PinaforeBisubstitutionM Identity
    mkbisub (MkAnyW vn) =
        MkBisubstitution
            vn
            (return $
             ccontramap (toEnhanced @_ @(PinaforeShim Type) "eliminated" $ \_ -> error "bad bisubstitution") $
             mkShimWit NilPinaforeType)
            (return $
             cfmap (toEnhanced @_ @(PinaforeShim Type) "eliminated" $ \_ -> error "bad bisubstitution") $
             mkShimWit NilPinaforeType)
    in toList $ fmap mkbisub $ posonlyvars <> negonlyvars

eliminateOneSidedTypeVars ::
       forall a. PShimWitMappable (PinaforeShim Type) PinaforeType a
    => a
    -> a
eliminateOneSidedTypeVars expr = runIdentity $ bisubstitutes (getEliminateBisubs expr) expr
