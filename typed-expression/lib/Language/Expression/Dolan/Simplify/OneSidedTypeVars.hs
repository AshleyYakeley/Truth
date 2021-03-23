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

eliminationBisubs ::
       forall (ground :: GroundTypeKind). IsDolanGroundType ground
    => (FiniteSet (AnyW SymbolType), FiniteSet (AnyW SymbolType))
    -> [Bisubstitution ground (DolanPolyShim ground Type) Identity]
eliminationBisubs (posvars, negvars) = let
    posbisub :: AnyW SymbolType -> Bisubstitution ground (DolanPolyShim ground Type) Identity
    posbisub (MkAnyW var) =
        assignUVarT @BottomType var $
        MkBisubstitution False var (return $ mkShimWit NilDolanType) (return $ varDolanShimWit var)
    negbisub :: AnyW SymbolType -> Bisubstitution ground (DolanPolyShim ground Type) Identity
    negbisub (MkAnyW var) =
        assignUVarT @TopType var $
        MkBisubstitution False var (return $ varDolanShimWit var) (return $ mkShimWit NilDolanType)
    in (fmap posbisub $ toList posvars) <> (fmap negbisub $ toList negvars)

eliminateVars ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanGroundType ground, PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) a)
    => (FiniteSet (AnyW SymbolType), FiniteSet (AnyW SymbolType))
    -> a
    -> a
eliminateVars vars expr = runIdentity $ bisubstitutes @ground (eliminationBisubs vars) expr

eliminateOneSidedTypeVars ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanGroundType ground, PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) a)
    => a
    -> a
eliminateOneSidedTypeVars expr = let
    (setFromList -> posvars, setFromList -> negvars) = mappableGetVars @ground expr
    posonlyvars :: FiniteSet _
    posonlyvars = difference posvars negvars
    negonlyvars :: FiniteSet _
    negonlyvars = difference negvars posvars
    in eliminateVars @ground (posonlyvars, negonlyvars) expr
