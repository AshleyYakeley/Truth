module Language.Expression.Dolan.Simplify.OneSidedTypeVars
    ( eliminateOneSidedTypeVars
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Simplify.VarUses
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

getEliminateBisubs ::
       forall (ground :: GroundTypeKind) t. IsDolanGroundType ground
    => (PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) t) =>
               t -> [Bisubstitution ground (DolanPolyShim ground Type) Identity]
getEliminateBisubs expr = let
    (setFromList -> posvars, setFromList -> negvars) = mappableGetVars @ground expr
    posonlyvars :: FiniteSet _
    posonlyvars = difference posvars negvars
    negonlyvars :: FiniteSet _
    negonlyvars = difference negvars posvars
    posbisub :: AnyW SymbolType -> Bisubstitution ground (DolanPolyShim ground Type) Identity
    posbisub (MkAnyW var) =
        assignUVar @Type @BottomType var $
        MkBisubstitution False var (return $ mkShimWit NilDolanType) (return $ varDolanShimWit var)
    negbisub :: AnyW SymbolType -> Bisubstitution ground (DolanPolyShim ground Type) Identity
    negbisub (MkAnyW var) =
        assignUVar @Type @TopType var $
        MkBisubstitution False var (return $ varDolanShimWit var) (return $ mkShimWit NilDolanType)
    in (fmap posbisub $ toList posonlyvars) <> (fmap negbisub $ toList negonlyvars)

eliminateOneSidedTypeVars ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanGroundType ground, PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) a)
    => a
    -> a
eliminateOneSidedTypeVars expr = runIdentity $ bisubstitutes @ground (getEliminateBisubs expr) expr
