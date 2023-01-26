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
    => (FiniteSet (Some SymbolType), FiniteSet (Some SymbolType))
    -> [Bisubstitution ground (DolanShim ground) Identity]
eliminationBisubs (posvars, negvars) = let
    posbisub :: Some SymbolType -> Bisubstitution ground (DolanShim ground) Identity
    posbisub (MkSome var) =
        assignUVarT @BottomType var $ MkBisubstitution False var (return nilDolanShimWit) (return $ varDolanShimWit var)
    negbisub :: Some SymbolType -> Bisubstitution ground (DolanShim ground) Identity
    negbisub (MkSome var) =
        assignUVarT @TopType var $ MkBisubstitution False var (return $ varDolanShimWit var) (return nilDolanShimWit)
    in (fmap posbisub $ toList posvars) <> (fmap negbisub $ toList negvars)

eliminateVars ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanGroundType ground, PShimWitMappable (DolanShim ground) (DolanType ground) a)
    => (FiniteSet (Some SymbolType), FiniteSet (Some SymbolType))
    -> Endo a
eliminateVars vars = endoMToEndo $ bisubstitutes @ground (eliminationBisubs vars)

eliminateOneSidedTypeVars ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanGroundType ground, PShimWitMappable (DolanShim ground) (DolanType ground) a)
    => Endo a
eliminateOneSidedTypeVars =
    Endo $ \expr -> let
        (setFromList -> posvars, setFromList -> negvars) = mappableGetVars @ground expr
        posonlyvars :: FiniteSet _
        posonlyvars = difference posvars negvars
        negonlyvars :: FiniteSet _
        negonlyvars = difference negvars posvars
        in appEndo (eliminateVars @ground (posonlyvars, negonlyvars)) expr
