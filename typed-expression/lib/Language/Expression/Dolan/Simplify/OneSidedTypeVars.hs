module Language.Expression.Dolan.Simplify.OneSidedTypeVars
    ( eliminateOneSidedTypeVars
    ) where

import Data.Shim
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Simplify.VarUses
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

getEliminateBisubs ::
       forall (ground :: GroundTypeKind) t. IsDolanGroundType ground
    => (PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) t) => t -> [Bisubstitution ground Identity]
getEliminateBisubs expr = let
    (setFromList -> posvars, setFromList -> negvars) = mappableGetVars @ground expr
    posonlyvars :: FiniteSet _
    posonlyvars = difference posvars negvars
    negonlyvars :: FiniteSet _
    negonlyvars = difference negvars posvars
    mkbisub :: AnyW SymbolType -> Bisubstitution ground Identity
    mkbisub (MkAnyW vn) =
        MkBisubstitution
            vn
            (return $
             ccontramap (toEnhanced @_ @(DolanPolyShim ground Type) "eliminated" $ \_ -> error "bad bisubstitution") $
             mkShimWit $ PlainDolanType NilDolanPlainType)
            (return $
             cfmap (toEnhanced @_ @(DolanPolyShim ground Type) "eliminated" $ \_ -> error "bad bisubstitution") $
             mkShimWit $ PlainDolanType NilDolanPlainType)
    in toList $ fmap mkbisub $ posonlyvars <> negonlyvars

eliminateOneSidedTypeVars ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanGroundType ground, PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) a)
    => a
    -> a
eliminateOneSidedTypeVars expr = runIdentity $ bisubstitutes @ground (getEliminateBisubs expr) expr
