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
       forall baseupdate t. (PShimWitMappable PinaforeShim (PinaforeType baseupdate) t)
    => t
    -> [PinaforeBisubstitutionM Identity baseupdate]
getEliminateBisubs expr = let
    (setFromList -> posvars, setFromList -> negvars) = mappableGetVars @baseupdate expr
    posonlyvars :: FiniteSet _
    posonlyvars = difference posvars negvars
    negonlyvars :: FiniteSet _
    negonlyvars = difference negvars posvars
    mkbisub :: AnyW SymbolType -> PinaforeBisubstitutionM Identity baseupdate
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
       forall baseupdate a. PShimWitMappable PinaforeShim (PinaforeType baseupdate) a
    => a
    -> a
eliminateOneSidedTypeVars expr = runIdentity $ bisubstitutes @baseupdate (getEliminateBisubs expr) expr
